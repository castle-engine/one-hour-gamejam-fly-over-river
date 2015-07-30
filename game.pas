{$mode objfpc}{$H+}

{ Implements the game logic, independent from mobile / standalone. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindow;

implementation

uses SysUtils,
  CastleFilesUtils, CastleKeysMouse,
  CastleImages, CastleGLImages, CastleVectors, CastleMessages;

type
  { Entity in a world, like a player or enemy or rocket. }
  TEntity = object
    Position: TVector2Single;
    Alive: boolean;
    Image: TGLImage;
    function Collides(const E: TEntity): boolean;
    function Width: Integer;
    function Height: Integer;
  end;

function TEntity.Collides(const E: TEntity): boolean;
begin
  Result :=
    Alive and
    E.Alive and
    { note: use "div 3", not "div 2" below, to collide only when things
       are really close, not when merely their bounding boxes collide. }
    (Abs(Position[0] - E.Position[0]) < (Width  + E.Width ) div 3) and
    (Abs(Position[1] - E.Position[1]) < (Height + E.Height) div 3);
end;

function TEntity.Width: Integer;
begin
  Result := Image.Width;
end;

function TEntity.Height: Integer;
begin
  Result := Image.Height;
end;

var
  Map, PlayerImage, Enemy, Rocket: TGLImage;
  MapMask: TGrayscaleImage;
  Player: TEntity;
  Enemies: array [0..50] of TEntity;
  Rockets: array [0..40] of TEntity;

procedure Restart;
var
  I: Integer;
begin
  Player.Alive := true;
  Player.Image := PlayerImage;
  Player.Position := Vector2Single(Map.Width / 2 - 100, Player.Height);

  for I := 0 to High(Enemies) do
  begin
    Enemies[I].Alive := true;
    Enemies[I].Image := Enemy;
    Enemies[I].Position := Vector2Single(
      Map.Width div 4 + Random * Map.Width / 2,
      Random * Map.Height * 3
    );
  end;

  for I := 0 to High(Rockets) do
  begin
    Rockets[I].Alive := false;
    Rockets[I].Image := Rocket;
  end;
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  Map := TGLImage.Create(ApplicationData('map.png'));
  MapMask := LoadImage(ApplicationData('map_mask.png'), [TGrayscaleImage]) as TGrayscaleImage;
  PlayerImage := TGLImage.Create(ApplicationData('SpaceShipSmall.png'));
  Enemy := TGLImage.Create(ApplicationData('ship6c.png'));
  Rocket := TGLImage.Create(ApplicationData('cohete_off.png'));

  Restart;
end;

{ Window.OnRender callback. }
procedure WindowRender(Container: TUIContainer);

  function ShiftY: Single;
  begin
    Result := -Player.Position[1] + 1.5 * Player.Height;
  end;

  procedure DrawEntity(const E: TEntity);
  begin
    if E.Alive then
      E.Image.Draw(
        Round(E.Position[0] - E.Width div 2),
        Round(E.Position[1] - E.Height div 2 + ShiftY)
      );
  end;

var
  I: Integer;
begin
  // TODO: couple of times render map
  for I := 0 to 3 do // TODO: unoptimal, only 2 draws should be needed
    Map.Draw(0, Round(ShiftY + Map.Height * I));

  DrawEntity(Player);

  for I := 0 to High(Enemies) do
    DrawEntity(Enemies[I]);

  for I := 0 to High(Rockets) do
  begin
    if Rockets[I].Alive and
       (Rockets[I].Position[1] > (Map.Height * 3)) then
        // TODO: better condition to end rocket fly
      Rockets[I].Alive := false;

    DrawEntity(Rockets[I]);
  end;
end;

{ Window.OnUpdate callback. }
procedure WindowUpdate(Container: TUIContainer);

  function PlayerCrashedWithWall(const X, Y: Single): boolean;
  const
    { For collisions of "player vs wall", we check points around player middle,
      but not too far around, to not detect collisions too soon.
      Set this to 1.0 to exactly check player bbox corners,
      set to something smaller to give more chance to avoid collisions. }
    PlayerMargin = 0.5;
  var
    MapX, MapY: Integer;
  begin
    MapX := Round(Player.Position[0] + X * PlayerMargin * Player.Width / 2);
    MapY := Round(Player.Position[1] + Y * PlayerMargin * Player.Height / 2);
    { is mask black }
    Result := MapMask.PixelPtr(MapX mod Map.Width, MapY mod Map.Height)^ = 0;
    if Result then
    begin
      MessageOk(Window, 'Crash!');
      Restart;
    end;
  end;

const
  { speeds, in map pixels per second }
  MoveSidewaysSpeed = 300;
  MoveForwardSpeed = 500;
  RocketSpeed = 2000;
var
  I, J: Integer;
begin
  { move player }
  Player.Position += Vector2Single(0, Window.Fps.UpdateSecondsPassed * MoveForwardSpeed);
  if Window.Pressed[K_Right] then
    Player.Position += Vector2Single(Window.Fps.UpdateSecondsPassed * MoveSidewaysSpeed, 0);
  if Window.Pressed[K_Left]  then
    Player.Position -= Vector2Single(Window.Fps.UpdateSecondsPassed * MoveSidewaysSpeed, 0);

  { collisions enemies vs player }
  for I := 0 to High(Enemies) do
    if Player.Collides(Enemies[I]) then
    begin
      MessageOk(Window, 'Crash with enemy!');
      Restart;
      Exit;
    end;

  { collisions rockets vs enemies, move rockets }
  for I := 0 to High(Rockets) do
    if Rockets[I].Alive then
    begin
      for J := 0 to High(Enemies) do
        if Enemies[J].Collides(Rockets[I]) then
          Enemies[J].Alive := false;

      Rockets[I].Position += Vector2Single(0, Window.Fps.UpdateSecondsPassed * RocketSpeed);
    end;

  { collisions player vs wall }
  if PlayerCrashedWithWall( 0,  0) then Exit;
  if PlayerCrashedWithWall(-1, -1) then Exit;
  if PlayerCrashedWithWall(-1,  1) then Exit;
  if PlayerCrashedWithWall( 1,  1) then Exit;
  if PlayerCrashedWithWall( 1, -1) then Exit;

  { we constantly change Player.Position, to just redraw constantly }
  Window.Invalidate;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
var
  I: Integer;
begin
  if Event.IsKey(K_Space) then
  begin
    for I := 0 to High(Rockets) do
      { fire first available rocket }
      if not Rockets[I].Alive then
      begin
        Rockets[I].Alive := true;
        Rockets[I].Position := Player.Position;
        Break;
      end;
  end;

  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
end;

function MyGetApplicationName: string;
begin
  Result := 'fly_over_river';
end;

initialization
  { This sets SysUtils.ApplicationName.
    It is useful to make sure it is correct (as early as possible)
    as our log routines use it. }
  OnGetApplicationName := @MyGetApplicationName;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
  { for now, just hardcode the size to match map width, looks best }
  Window.Width := 1024;
  Window.Height := 1024;
  Window.ResizeAllowed := raNotAllowed;
  Window.OnRender := @WindowRender;
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
finalization
  FreeAndNil(Map);
  FreeAndNil(MapMask);
  FreeAndNil(PlayerImage);
  FreeAndNil(Enemy);
  FreeAndNil(Rocket);
end.
