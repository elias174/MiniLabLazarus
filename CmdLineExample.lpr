program CmdLineExample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg
  { add your units here }, wnmainform, cmdbox, solver_eq, integral_solver,
  ParseMath, main, diferential_solver, utils_commands, Matrix,
  generalized_solver_eq, Lagrange;

begin
  Application.Initialize;
  Application.CreateForm(TWMainForm, WMainForm);
  Application.CreateForm(TfrmGraficadora, frmGraficadora);
  Application.Run;
end.

