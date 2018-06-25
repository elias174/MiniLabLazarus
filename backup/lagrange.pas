unit Lagrange;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Graphics, FileUtil, Forms, Controls, ExtCtrls,
    Grids, math, ParseMath, matrix;


type
   TFooRecLagrange = record
    S: String;
    B: Boolean;
    V: Real;
   end;
   LagrangePol = class
       polynom_generated: String;
       max_x: Real;
       min_x: Real;
       Parse: TParseMath;
       Parse_compare: TParseMath;
       x_points: array of Real;
       y_points: array of Real;
       function get_lagrandian(i: Integer): String;
       function generate_function(): String;
       function eval_generated_function(x: Real): TFooRecLagrange;
       procedure read_points_from_grid(grid_points: TStringGrid );
       procedure read_points_from_matrix(matrix_points: TMatrix );
       //public
       //    constructor create;
end;
implementation
procedure PrintArray(input : array of Real);
var
   i : integer;
begin
    for i := 1 to length(input) do
       write(input[i - 1],' ');
    writeln;
end;

procedure LagrangePol.read_points_from_matrix(matrix_points: TMatrix);
var i, n_cols: Integer;
begin
  SetLength(x_points, Length(matrix_points.A));
  SetLength(y_points, Length(matrix_points.A));
  for i:=0 to Length(matrix_points.A) do
  begin
    x_points[i] := 1;
    y_points[i] := 1;
  end;
  max_x:= maxvalue(x_points);
  min_x:= minvalue(x_points);

end;

procedure LagrangePol.read_points_from_grid(grid_points: TStringGrid);
var i, n_cols: Integer;
begin
  n_cols:= grid_points.ColCount;
  SetLength(x_points, n_cols-1);
  SetLength(y_points, n_cols-1);
  for i:=1 to n_cols-1 do
  begin
    x_points[i-1] := StrToFloat(grid_points.Cells[i,0]);
    y_points[i-1] := StrToFloat(grid_points.Cells[i,1]);
  end;
  max_x:= maxvalue(x_points);
  min_x:= minvalue(x_points);

end;

function LagrangePol.eval_generated_function(x: Real): TFooRecLagrange;
var foo_cos: String;
   error_abs: Real;
   to_calc: Real;
   i: Integer;
   ev1, ev2: Real;
begin
  Result.B:= True;
  Result.S:= '';
  to_calc:=0;

  Parse:= TParseMath.create();
  Parse.Expression:= polynom_generated;
  Parse.AddVariable('x', x);
  Result.V:= Parse.Evaluate();

end;

function LagrangePol.get_lagrandian(i: Integer): String;
var xi, denominator: Real;
  numerator: String;
  j: Integer;
begin
  Result:= '';
  denominator:= 1;
  numerator:= '1';
  xi:= x_points[i];
  for j:=0 to (Length(x_points)-1) do
  begin
    if not (i = j) then begin
      numerator:= numerator + '*(x-' + FloatToStr(x_points[j]) + ')';
      denominator:= denominator * (xi-x_points[j]);
    end;
  end;
  Result:= '(' + numerator + '/' + '(' + FloatToStr(denominator) + ')' + ')';
end;

function LagrangePol.generate_function(): String;
var i:Integer;
begin
  Result := '0';
  for i:=0 to (Length(x_points)-1) do
  begin
    Result:= Result + '+' + '(' + FloatToStr(y_points[i]) + '*' + get_lagrandian(i) + ')';
  end;
  polynom_generated:=Result;
end;

end.

