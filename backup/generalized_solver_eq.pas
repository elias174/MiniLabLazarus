unit generalized_solver_eq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Matrix,ParseMath, Grids, math;
type
   m_Matrix_functions = array of array of String;
type
   GeneralSolverEq = class
       jacobian: TMatrix;
       variables: m_Matrix_functions;
       functions: m_Matrix_functions;
       values_x0: TMatrix;
       Parse: TParseMath;
       Error: Real;
       H_variable: Real;
       ErrorAllowed: Real;
       Sequence_xn: TStringList;
       Sequence_error:TStringList;
       procedure read_from_grids(ma, mv: TStringGrid );
       procedure read_from_string_matrix(str_functions, str_variables: TStringList; points_values: TMatrix; VAR success: Boolean);
       function eval_jacobian(values: TMatrix): TMatrix;
       function eval_function(values: TMatrix): TMatrix;
       function newthon_rapsody(): TMatrix;
       function get_next_xn(xn: TMatrix): TMatrix;
       public
           constructor create;
   end;

implementation
const
  Top = 100000;

constructor GeneralSolverEq.create;
begin

     Sequence_xn:= TStringList.Create;
     Sequence_error:= TStringList.Create;
     Sequence_xn.Add('');
     Sequence_error.Add('');
     Error:= 1;
end;

procedure GeneralSolverEq.read_from_grids(ma, mv: TStringGrid);
var
  m,n,j,i, tmp_j:Integer;
begin
  values_x0:=  TMatrix.Create(mv.ColCount, 1);
  jacobian:=  TMatrix.Create(ma.RowCount, mv.ColCount);
  SetLength(functions, ma.RowCount, 1);
  SetLength(variables, 1, mv.ColCount);

  //m:=Length(jacobian);n:=Length(jacobian[0]);
  //for i:=0 to m-1 do
  //    for j:=0 to n-1 do
  //        begin
  //             jacobian[i,j] := ma.Cells[j+1,i];
  //        end;
  m:=Length(functions);
  for i:=0 to m-1 do
       functions[i,0] := ma.Cells[0,i];

  m:=Length(variables[0]);
  for i:=0 to m-1 do
       variables[0,i] := mv.Cells[i,0];

  m:=Length(values_x0.A);
  for i:=0 to m-1 do
      values_x0.A[i,0] := StrToFloat(mv.Cells[i,1]);
end;

procedure GeneralSolverEq.read_from_string_matrix(str_functions, str_variables: TStringList; points_values: TMatrix; VAR success: Boolean);
var
  m,n,j,i, tmp_j:Integer;
begin
  success:= True;
  if (str_functions.Count <> str_variables.Count) or (Length(points_values.A[0]) <> str_variables.Count) or (Length(points_values.A[0]) <> str_functions.Count) then
  begin
     success:= False;
     Exit;
  end;
  values_x0:=  TMatrix.Create(str_variables.Count, 1);
  jacobian:=  TMatrix.Create(str_functions.Count, str_variables.Count);
  SetLength(functions, str_functions.Count, 1);
  SetLength(variables, 1, str_variables.Count);

  //m:=Length(jacobian);n:=Length(jacobian[0]);
  //for i:=0 to m-1 do
  //    for j:=0 to n-1 do
  //        begin
  //             jacobian[i,j] := ma.Cells[j+1,i];
  //        end;
  m:=Length(functions);
  for i:=0 to m-1 do begin
       functions[i,0] := copy(str_functions[i],2,str_functions[i].Length - 2);
  end;


  m:=Length(variables[0]);
  for i:=0 to m-1 do begin
       variables[0,i] := str_variables[i];
  end;

  m:=Length(values_x0.A);
  for i:=0 to m-1 do
      values_x0.A[i,0] := points_values.A[0,i];
end;

function GeneralSolverEq.eval_jacobian(values: TMatrix): TMatrix;
var i,j,k,m,n,size_variables, variable_i: Integer;
  tmp_value, first_f, second_f: Real;
  res:TMatrix;
  tmp_function, tmp_variable: String;
begin
  m:=Length(jacobian.A);n:=Length(jacobian.A[0]);
  res := TMatrix.Create(m,n);
  for i:=0 to m-1 do
  begin
       for j:=0 to n-1 do
       begin
            //add h to x0_j
            tmp_value:=values.A[0,j];
            values.A[0,j] := values.A[0,j] + H_variable;
            tmp_function:= functions[i][0];

            Parse:= TParseMath.create();
            Parse.Expression:= tmp_function;
            for variable_i:=0 to Length(variables[0])-1 do
            begin
                 Parse.AddVariable(variables[0][variable_i], values.A[0][variable_i]);
            end;
            first_f:= Parse.Evaluate();

            values.A[0,j] := tmp_value;
            Parse:= TParseMath.create();
            Parse.Expression:= tmp_function;
            for variable_i:=0 to Length(variables[0])-1 do
            begin
                 Parse.AddVariable(variables[0][variable_i], values.A[0][variable_i]);
            end;
            second_f:= Parse.Evaluate();

            res.A[i,j]:= (first_f - second_f) / H_variable;
       end;
  end;
  Result := res;
end;

function GeneralSolverEq.eval_function(values: TMatrix): TMatrix;
var i,j,m,n, variable_i: Integer;
  tmp_value: Real;
  res:TMatrix;
  tmp_function, tmp_variable: String;
  Parse_3: TParseMath;
begin
  m:=Length(functions);n:=Length(functions[0]);
  res := TMatrix.Create(m,n);
  for i:=0 to m-1 do
  begin
       for j:=0 to n-1 do
       begin
            tmp_function:= functions[i][j];
            Parse_3:= TParseMath.create();
            Parse_3.Expression:= tmp_function;
            WriteLn('For expression ' + tmp_function);
            for variable_i:=0 to Length(variables[0])-1 do
            begin
                 Parse_3.AddVariable(variables[0][variable_i], values.A[0][variable_i]);
                 Write(variables[0][variable_i]);
                 Write(' with ');
                 Write(values.A[0][variable_i]);
                 WriteLn();
            end;
            tmp_value := Parse_3.Evaluate();
            WriteLn('Result: '+FloatToStr(tmp_value));
            WriteLn();
            res.A[i,j] := tmp_value;
       end;
  end;
  Result := res;
end;

function GeneralSolverEq.get_next_xn(xn: TMatrix): TMatrix;
var res:TMatrix;
  to_sub: TMatrix;
  jacobian_evalued, function_evalued: TMatrix;
  multiplication: TMatrix;
begin
  jacobian_evalued:= eval_jacobian(xn);
  jacobian_evalued:= jacobian_evalued.MInverse(jacobian_evalued);
  function_evalued:= eval_function(xn);
  multiplication:= function_evalued.MMultiply(jacobian_evalued, function_evalued);
  to_sub := xn.MTranspose(xn);
  res:= to_sub.MResta(multiplication);
  Result:= res;
end;

function Norm_Matrix(matrix: TMatrix): Real;
var i, m:Integer;
  acum: Real;
begin
  m:= Length(matrix.A);
  acum:=0;
  for i:=0 to m-1 do
  begin
       acum := acum + power(matrix.A[i][0], 2);
  end;
  Result := sqrt(acum);
end;

function GeneralSolverEq.newthon_rapsody(): TMatrix;
var xn: TMatrix;
  tmp_xn: TMatrix;
  n: Integer;

begin
  xn:= values_x0;
  Result:=xn;
  Sequence_error.Add(FloatToStr(Error));
  n:=1;

  repeat
        Sequence_xn.Add(xn.To_String() );
        Result:= xn;
        xn:= xn.MTranspose(xn);
        xn:= get_next_xn(xn);
        if n > 0 then begin
           Error:=abs(Norm_Matrix(Result) - Norm_Matrix(xn));
           Sequence_error.Add( FloatToStr(Error) );
        end;
        n:= n+1;
  until ( Error <= ErrorAllowed ) or ( n >= Top );
end;

end.

