{$mode objfpc}{$H+}
{$apptype GUI}
program fly_over_river_standalone;
{$ifdef MSWINDOWS} {$R automatic-windows-resources.res} {$endif MSWINDOWS}
uses CastleWindow, Game;
begin
  Window.OpenAndRun;
end.
