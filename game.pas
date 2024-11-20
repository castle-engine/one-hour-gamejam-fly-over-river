{$mode objfpc}{$H+}

{ Implements the game logic, independent from mobile / standalone. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindow;

implementation

uses SysUtils,
  CastleFilesUtils, CastleKeysMouse, CastleTimeUtils, CastleControls,
  CastleImages, CastleGLImages, CastleVectors, CastleMessages, CastleColors,
  CastleApplicationProperties, CastleUiControls;

type
  { Entity in a world, like a player or enemy or rocket. }
  TEntity = object
    Position: TVector2;
    Alive: boolean;
    Image: TDrawableImage;
    DieTime: TFloatTime;
    DieAnimation: TGLVideo2D;
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
  Map, PlayerImage, Enemy, Rocket: TDrawableImage;
  MapMask: TGrayscaleImage;
  Player: TEntity;
  Enemies: array [0..50] of TEntity;
  Rockets: array [0..40] of TEntity;
  EnemiesDestroyed: Cardinal;
  CurrentTime: TFloatTime;
  Explosion: TGLVideo2D;

procedure Restart;
var
  I: Integer;
begin
  Player.Alive := true;
  Player.Image := PlayerImage;
  Player.Position := Vector2(Map.Width / 2 - 100, Player.Height);

  for I := 0 to High(Enemies) do
  begin
    Enemies[I].Alive := true;
    Enemies[I].Image := Enemy;
    Enemies[I].DieAnimation := Explosion;
    Enemies[I].Position := Vector2(
      Map.Width div 4 + Random * Map.Width / 2,
      Random * Map.Height * 3
    );
  end;

  for I := 0 to High(Rockets) do
  begin
    Rockets[I].Alive := false;
    Rockets[I].Image := Rocket;
  end;

  EnemiesDestroyed := 0;
  CurrentTime := 0;
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  Map := TDrawableImage.Create('castle-data:/map.png');
  MapMask := LoadImage('castle-data:/map_mask.png', [TGrayscaleImage]) as TGrayscaleImage;
  PlayerImage := TDrawableImage.Create('castle-data:/SpaceShipSmall.png');
  Enemy := TDrawableImage.Create('castle-data:/ship6c.png');
  Rocket := TDrawableImage.Create('castle-data:/cohete_off.png');
  //Explosion := TGLVideo2D.Create('castle-data:/explosion_320x240/explosion_1@counter(4).png', false);
  Explosion := TGLVideo2D.Create('castle-data:/explosion_320x240_frameskip2/explosion_1@counter(4).png', false);

  Restart;
end;

type
  { View to contain whole UI and to handle events, like updates. }
  TMyView = class(TCastleView)
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure Render; override;
  end;

procedure TMyView.Render;

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
      ) else
    if (E.DieAnimation <> nil) and
       (E.DieTime <> 0) and
       (CurrentTime < E.DieTime + E.DieAnimation.Duration) then
      E.DieAnimation.DrawableImageFromTime(CurrentTime - E.DieTime).Draw(
        Round(E.Position[0] - E.DieAnimation.Width div 2),
        Round(E.Position[1] - E.DieAnimation.Height div 2 + ShiftY)
      );
  end;

var
  I: Integer;
  S: string;
const
  { how soon to disappear the rocket, to make Rockets[] slots available,
    and to avoid killing enemies far away.
    Equal to Window.Width and Map.Width now, although does not have to. }
  RocketDisappearDistance = 1024;
begin
  inherited;

  // TODO: couple of times render map
  for I := 0 to 3 do // TODO: unoptimal, only 2 draws should be needed
    Map.Draw(0, Round(ShiftY + Map.Height * I));

  DrawEntity(Player);

  for I := 0 to High(Enemies) do
    DrawEntity(Enemies[I]);

  for I := 0 to High(Rockets) do
  begin
    if Rockets[I].Alive and
       (Rockets[I].Position[1] > Player.Position[1] + RocketDisappearDistance) then
      Rockets[I].Alive := false;

    DrawEntity(Rockets[I]);
  end;

  S := Format('Survided: %fs', [CurrentTime]);
  UIFont.Print(10, Container.PixelsHeight - 10 - UIFont.Height,
    LightGreen, S);
  S := Format('Destroyed: %d', [EnemiesDestroyed]);
  UIFont.Print(
    Container.PixelsWidth - 10 - UIFont.TextWidth(S),
    Container.PixelsHeight - 10 - UIFont.Height,
    Yellow, S);
end;

procedure TMyView.Update(const SecondsPassed: Single; var HandleInput: Boolean);

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
  inherited;

  CurrentTime += SecondsPassed;

  { move player }
  Player.Position += Vector2(0, SecondsPassed * MoveForwardSpeed);
  if Container.Pressed[keyArrowRight] then
    Player.Position += Vector2(SecondsPassed * MoveSidewaysSpeed, 0);
  if Container.Pressed[keyArrowLeft]  then
    Player.Position -= Vector2(SecondsPassed * MoveSidewaysSpeed, 0);

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
        begin
          Enemies[J].Alive := false;
          Enemies[J].DieTime := CurrentTime;
          Rockets[I].Alive := false;
          Inc(EnemiesDestroyed);
          Break;
        end;

      Rockets[I].Position += Vector2(0, SecondsPassed * RocketSpeed);
    end;

  { collisions player vs wall }
  if PlayerCrashedWithWall( 0,  0) then Exit;
  if PlayerCrashedWithWall(-1, -1) then Exit;
  if PlayerCrashedWithWall(-1,  1) then Exit;
  if PlayerCrashedWithWall( 1,  1) then Exit;
  if PlayerCrashedWithWall( 1, -1) then Exit;
end;

function TMyView.Press(const Event: TInputPressRelease): Boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keySpace) then
  begin
    for I := 0 to High(Rockets) do
      { fire first available rocket }
      if not Rockets[I].Alive then
      begin
        Rockets[I].Alive := true;
        Rockets[I].Position := Player.Position;
        Break;
      end;

    Exit(true);
  end;

  if Event.IsKey(keyF5) then
  begin
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
    Exit(true);
  end;
end;

var
  MyView: TMyView;
initialization
  ApplicationProperties.ApplicationName := 'fly_over_river';

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
  { TODO: just hardcode the size to match map width, looks best.
    This is 1-hour gamejam game! }
  Window.Width := 1024;
  Window.Height := 1024;
  Window.ResizeAllowed := raNotAllowed;

  MyView := TMyView.Create(Application);
  Window.Container.View := MyView;

  Application.MainWindow := Window;
finalization
  FreeAndNil(Map);
  FreeAndNil(MapMask);
  FreeAndNil(PlayerImage);
  FreeAndNil(Enemy);
  FreeAndNil(Rocket);
  FreeAndNil(Explosion);
end.
