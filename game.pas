{$mode objfpc}{$H+}

{ Implements the game logic, independent from mobile / standalone. }
unit Game;

interface

uses CastleWindowTouch;

var
  Window: TCastleWindowTouch;

implementation

uses SysUtils, CastleWindow, CastleScene, CastleControls,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse,
  CastleImages, CastleGLImages, CastleVectors, CastleMessages;

type
  TEnemy = object
    X, Y: Integer;
    Alive: boolean;
  end;

var
  MapWidth, MapHeight: Integer;
  Map, Player, Enemy, Rocket: TGLImage;
  MapMask: TCastleImage;
  Position: TVector2Single;
  Enemies: array [0..50] of TEnemy;
  Rockets: array [0..40] of TEnemy;

{ routines ------------------------------------------------------------------- }

procedure Restart;
var
  I: Integer;
begin
  Position := Vector2Single(MapWidth / 2 - 100, Player.Height / 2);
  for I := 0 to High(Enemies) do
  begin
    Enemies[I].Alive := true;
    Enemies[I].X := MapWidth div 4 +  Round(Random * MapWidth / 2);
    Enemies[I].Y := Round(Random * MapHeight * 3);
  end;

  for I := 0 to High(Rockets) do
    Rockets[I].Alive := false;
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  Map := TGLImage.Create(ApplicationData('map.png'));
  MapMask := LoadImage(ApplicationData('map_mask.png'), []);
  MapWidth := Map.Width;
  MapHeight := Map.Height;

  Player := TGLImage.Create(ApplicationData('SpaceShipSmall.png'));

  Enemy := TGLImage.Create(ApplicationData('ship6c.png'));

  Rocket := TGLImage.Create(ApplicationData('cohete_off.png'));

  Restart;
end;


procedure WindowResize(Container: TUIContainer);
begin
  // ... react to Container Width / Height changes
end;

procedure WindowRender(Container: TUIContainer);
var
  I, X, Y: Integer;
begin
  // TODO: couple of times render map
  for I := 0 to 3 do // TODO: unoptimal, only 2 draws should be needed
    Map.Draw(0, -Round(Position[1]) + Map.Height * I);
  Player.Draw(Round(Position[0]), Player.Height div 2);

  for I := 0 to High(Enemies) do
    if Enemies[I].Alive then
    begin
      Enemy.Draw(
        Enemies[I].X - Enemy.Width div 2,
        Enemies[I].Y - Enemy.Height div 2 +
        -Round(Position[1]));
    end;

  for I := 0 to High(Rockets) do
    if Rockets[I].Alive then
    begin
      if Rockets[I].Y > (MapHeight * 3) then
        // TODO: better condition to end rocket fly
        Rockets[I].Alive := false else
      begin
        X := Rockets[I].X - Rocket.Width div 2;
        Y := Rockets[I].Y - Rocket.Height div 2 - Round(Position[1]);
        Rocket.Draw(X, Y);
      end;
    end;
end;

procedure WindowUpdate(Container: TUIContainer);
var
  X, Y: Integer;
  I, J: Integer;
begin
  Position := Vector2Single(
    Position[0],
    Position[1] + Window.Fps.UpdateSecondsPassed * 500);
  Window.Invalidate;

//  if Window.Pressed[K_Up]    then ViewMoveY -= ViewMoveChangeSpeed * Window.Fps.UpdateSecondsPassed;
//  if Window.Pressed[K_Down]  then ViewMoveY += ViewMoveChangeSpeed * Window.Fps.UpdateSecondsPassed;

  if Window.Pressed[K_Right] then
    Position := Vector2Single(
      Position[0] + Window.Fps.UpdateSecondsPassed * 300,
      Position[1]);
  if Window.Pressed[K_Left]  then
    Position := Vector2Single(
      Position[0] - Window.Fps.UpdateSecondsPassed * 300,
      Position[1]);

  X := Round(Position[0] + Player.Width div 2);
  Y := Round(Position[1] + Player.Height div 2);
  for I := 0 to High(Enemies) do
    if Enemies[I].Alive and
       (Abs(Enemies[I].X - X) < (Player.Width + Enemy.Width) div 3) and
       (Abs(Enemies[I].Y - Y) < (Player.Height + Enemy.Height) div 3) then
    begin
      MessageOk(Window, 'Crash with enemy!');
      Restart;
    end;

  for I := 0 to High(Rockets) do
    if Rockets[I].Alive then
    begin
      for J := 0 to High(Enemies) do
        if Enemies[J].Alive and
           (Abs(Enemies[J].X - Rockets[I].X) < (Rocket.Width + Enemy.Width) div 3) and
           (Abs(Enemies[J].Y - Rockets[I].Y) < (Rocket.Height + Enemy.Height) div 3) then
          Enemies[J].Alive := false;

      Rockets[I].Y += Round(Window.Fps.UpdateSecondsPassed * 2000);
    end;

  X := Round(Position[0]);
  Y := Round(Position[1]);
  if //(X < 0) or (X >= MapMask.Width) or
     //(Y < 0) or (Y >= MapMask.Height) or
     { black }
     (PVector3Byte(MapMask.PixelPtr(X mod MapWidth, Y mod MapHeight))^[0] = 0) then
  begin
    MessageOk(Window, 'Crash!');
    Restart;
  end;

  X := Round(Position[0] + Player.Width * 0.6);
  Y := Round(Position[1] + Player.Height * 0.6);
  if //(X < 0) or (X >= MapMask.Width) or
     //(Y < 0) or (Y >= MapMask.Height) or
     { black }
     (PVector3Byte(MapMask.PixelPtr(X mod MapWidth, Y mod MapHeight))^[0] = 0) then
  begin
    MessageOk(Window, 'Crash!');
    Restart;
  end;

  X := Round(Position[0]);
  Y := Round(Position[1] + Player.Height * 0.6);
  if //(X < 0) or (X >= MapMask.Width) or
     //(Y < 0) or (Y >= MapMask.Height) or
     { black }
     (PVector3Byte(MapMask.PixelPtr(X mod MapWidth, Y mod MapHeight))^[0] = 0) then
  begin
    MessageOk(Window, 'Crash!');
    Restart;
  end;

  X := Round(Position[0] + Player.Width * 0.75);
  Y := Round(Position[1] + Player.Height * 0.25);
  if //(X < 0) or (X >= MapMask.Width) or
     //(Y < 0) or (Y >= MapMask.Height) or
     { black }
     (PVector3Byte(MapMask.PixelPtr(X mod MapWidth, Y mod MapHeight))^[0] = 0) then
  begin
    MessageOk(Window, 'Crash!');
    Restart;
  end;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
var
  I: Integer;
begin
  if Event.IsKey(K_Space) then
  begin
    for I := 0 to High(Rockets) do
      if not Rockets[I].Alive then
      begin
        Rockets[I].Alive := true;
        Rockets[I].X := Round(Position[0] + Player.Width div 2);
        Rockets[I].Y := Round(Position[1] + Player.Height div 2);
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
  Window := TCastleWindowTouch.Create(Application);
  Application.MainWindow := Window;
  Window.Width := 1024; // TODO: don't hardcode
  Window.Height := 1024;
  // todo res allowed none
  Window.OnRender := @WindowRender;
  Window.OnResize := @WindowResize;
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
finalization
  FreeAndNil(Map);
  FreeAndNil(MapMask);
  FreeAndNil(Player);
  FreeAndNil(Enemy);
  FreeAndNil(Rocket);
end.
