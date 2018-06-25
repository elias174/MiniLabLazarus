{ Copyright (C) 2007 Julian Schutsch

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
  
  This Software is GPL, not LGPL as the libary it uses !
  
  Changelog
    10.8.2007 : Added "Buttons" Unit to avoid "TButton" missing error on 0.9.22 (Linux)

}
unit wnmainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Graphics, Dialogs, ExtCtrls, LCLType,
  ucmdbox, StdCtrls, Controls, Buttons, Menus, ComCtrls, TAGraph, TASeries,
  TAFuncSeries, solver_eq, fpexprpars, math, integral_solver,
  diferential_solver, main, RegExpr, fgl, LCLIntf, Matrix, strutils, utils_commands,
  generalized_solver_eq, Lagrange, TAChartUtils, TAStyles, TAChartStrConsts, ParseMath, TATypes;

type
  PTMatrix = ^TMatrix;

  PairValueType = record
    value: String;
    m_type: String;
  end;
  TMClassMap = specialize TFPGMap<String,PairValueType>;
  TMatrixClassMap = specialize TFPGMap<String,TMatrix>;
  TArrayClassMap = specialize TFPGMap<String,TStringList>;
  FunctionDynamicClass = class
    fx: String;
    parser_functions: TParseMath;
    procedure calculate_function(const AX: Double; out AY: Double);
    public
      constructor create(foo_x: String);
  end;


  { TWMainForm }
  TWMainForm = class(TForm)
    functions_list_to_calc: TStringList;
    Area: TAreaSeries;
    argument: TFPExprIdentifierDef;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    cbWordWrap: TCheckBox;
    CbSetCaret: TComboBox;
    chkProportional1: TCheckBox;
    chrGrafica: TChart;
    CmdBox: TCmdBox;
    colorbtnFunction1: TColorButton;
    EjeX: TConstantLine;
    EjeY: TConstantLine;
    FontDialog: TFontDialog;
    Funcion: TFuncSeries;
    Label1: TLabel;
    HistoryList: TListBox;
    MenuItem1: TMenuItem;
    OperationPanel1: TPanel;
    Plotear: TLineSeries;
    PopupMenu1: TPopupMenu;
    RightPanel: TPanel;
    OperationPanel: TPanel;
    Splitter1: TSplitter;
    ReaderTimer: TTimer;
    ProcessTimer: TTimer;
    Splitter2: TSplitter;
    trbMax1: TTrackBar;
    trbMin1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cbWordWrapChange(Sender: TObject);
    procedure chkProportional1Change(Sender: TObject);
    procedure CmdBoxInput(ACmdBox: TCmdBox; Input: String);
    procedure CbSetCaretChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OperationPanelClick(Sender: TObject);
    procedure ProcessTimerTimer(Sender: TObject);
    procedure ReaderTimerTimer(Sender: TObject);

    procedure calculate_function(const AX: Double; out AY: Double);
    procedure graph_function(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
    procedure diferential_solver(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
    procedure lineal_solver(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
    procedure intersect_lineal_solver(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
    procedure integral(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
    procedure solver_area(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
    function check_for_memory(input: String): Boolean;
    function resolve_matrix_or_array_content(key,value: String; VAR ret_pointer: Pointer; VAR type_ret: String): Boolean;
    function register_in_memory(key, value: String): Boolean;
    procedure resolve_function(input: String; VAR result_resolve : String);
    function get_from_memory(key: String; VAR exist: Boolean; VAR type_str: String): String;
    procedure trbMax1Change(Sender: TObject);
    procedure trbMin1Change(Sender: TObject);

  private
    parser_functions: TParseMath;
    FunctionList: TList;
    Memory: TMClassMap;
    Memory_Matrix: TMatrixClassMap;
    Memory_Arrays: TArrayClassMap;
    FParser: TFPExpressionParser;
    identifier: array of TFPExprIdentifierDef;

    TextPosition : Integer;
    DText        : TStringList;
    Rdpw         : Boolean;
    FProcess     : Integer;
  end; 

var WMainForm: TWMainForm;


implementation
var Dir:String;

{ TWMainForm }

procedure TWMainForm.calculate_function(const AX: Double; out AY: Double);
begin
  parser_functions.NewValue('x',AX);
  AY:=parser_functions.Evaluate();
end;

procedure FunctionDynamicClass.calculate_function(const AX: Double; out AY: Double);
begin
  parser_functions.NewValue('x',AX);
  AY:=parser_functions.Evaluate();
end;

constructor FunctionDynamicClass.create(foo_x: String);
begin
  fx:=foo_x;
 parser_functions:=TParseMath.create();
 parser_functions.Expression:=fx;
 parser_functions.AddVariable('x',0.0);
end;

procedure TWMainForm.graph_function(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  it_function: Real;
  parse_function: TParseMath;
  fx: String;
  a,b: Double;
begin
 fx := Args[0].ResString;
 a := ArgToFloat(Args[1]);
 b := ArgToFloat(Args[2]);
 FunctionList.Add( TLineSeries.Create( chrGrafica ) );
 parse_function:= TParseMath.create();
 parse_function.Expression:= fx;
 parse_function.AddVariable('x',0.0);
 with TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'LineSeries' + IntToStr( FunctionList.Count );
      LinePen.Color:= colorbtnFunction1.ButtonColor;
      LinePen.Width:= 1;
      Tag:=0;
      Active:= True;
      ShowLines:=True;
      it_function:=a;
      while it_function <= b do
      begin
           parse_function.NewValue('x',it_function);
           AddXY(it_function,parse_function.Evaluate());
           it_function:=it_function+0.001;
      end;
 end;
 chrGrafica.AddSeries( TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) );
 Result.ResFloat:=0.0;
end;

procedure TWMainForm.integral(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  solver_integral: IntegralSolver;
  graph: TfrmGraficadora;
  a,b: Double;
  n, method: Integer;
  fx: String;
  it_integral: Real;
  parse_area: TParseMath;
  dyn_calc:  FunctionDynamicClass;

begin

 fx := Args[0].ResString;
 a := ArgToFloat(Args[1]);
 b := ArgToFloat(Args[2]);
 n := Args[3].ResInteger;
 method := Args[4].ResInteger;

 parse_area:=TParseMath.create();
 parse_area.AddVariable('x',0.0);
 solver_integral := IntegralSolver.create;
 dyn_calc:=FunctionDynamicClass.Create(fx);
 case method of
  0: begin
   Result.ResFloat:= solver_integral.trapezoidal_method(n,a,b,fx);
  end;
  1: begin
   Result.ResFloat:= solver_integral.simpson_method(n,a,b,fx);
  end;
  2: begin
   Result.ResFloat:= solver_integral.simpson_method_3_8(n,a,b,fx);
  end;
 end;

 WriteLn(FloatToStr(solver_integral.local_h));
 parser_functions.Expression:=fx;
 parse_area.Expression:=fx;
 FunctionList.Add( TFuncSeries.Create( chrGrafica ) );
 with TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'FuncSeries' + IntToStr( FunctionList.Count );
      Pen.Color:= colorbtnFunction1.ButtonColor;
      Pen.Width:= 1;
      Tag:=0;
      Active:= True;
      OnCalculate:=@dyn_calc.calculate_function;
 end;
 chrGrafica.AddSeries( TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) );
 FunctionList.Add( TAreaSeries.Create( chrGrafica ) );
 with TAreaSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'FuncSeries' + IntToStr( FunctionList.Count );
      AreaBrush.Color:=$00E1E1FF;
      AreaContourPen.Color:=clRed;
      AreaContourPen.Cosmetic:=True;
      AreaContourPen.Style:=psDot;
      AreaLinesPen.Style:=psClear;
      AxisIndexX:=0;
      Tag:=0;
      Active:= True;
      Transparency:=150;
      Stacked:=True;
      it_integral:=a;
      UseZeroLevel:=True;
      while it_integral<=b do begin
           parse_area.NewValue('x',it_integral);
           AddXY(it_integral,parse_area.Evaluate());
           it_integral:=it_integral+solver_integral.local_h;
      end;
 end;
 chrGrafica.AddSeries( TAreaSeries( FunctionList[ FunctionList.Count - 1 ] ) );




end;

procedure TWMainForm.lineal_solver(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  solver_linear: SolverEq;
  a,b: Real;
  method: Integer;
  tmp_result: Real;
  fx: String;
  dyn_calc:  FunctionDynamicClass;
begin
 fx := Args[0].ResString;
 a := ArgToFloat(Args[1]);
 b := ArgToFloat(Args[2]);
 method := Args[3].ResInteger;
 solver_linear:=SolverEq.create;
 solver_linear.fx:=fx;
 solver_linear.solve_a:=a;
 solver_linear.solve_b:=b;
 solver_linear.solve_h:=0.00001;
 solver_linear.ErrorAllowed:=0.0001;
 solver_linear.MethodType:=method;
 if not solver_linear.validate().B then begin
  Result.ResFloat:=NaN;
  CmdBox.Writeln('Error: ' + solver_linear.validate().S);
  Exit;
 end;
 tmp_result:=solver_linear.execute();

 dyn_calc:=FunctionDynamicClass.Create(fx);
 FunctionList.Add( TFuncSeries.Create( chrGrafica ) );
 with TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'FuncSeries' + IntToStr( FunctionList.Count );
      Pen.Color:= colorbtnFunction1.ButtonColor;
      Pen.Width:= 1;
      Tag:=0;
      Active:= True;
      OnCalculate:=@dyn_calc.calculate_function;
 end;

 chrGrafica.AddSeries( TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) );

 FunctionList.Add( TLineSeries.Create( chrGrafica ) );
 with TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'LineSeries' + IntToStr( FunctionList.Count );
      LinePen.Color:= clRed;
      LinePen.Width:= 1;
      LinePen.Style:=psDot;
      Pointer.Style:=psCircle;
      Pointer.Brush.Color:=clRed;
      Tag:=0;
      Active:= True;
      ShowLines:=False;
      ShowPoints:=True;
      AddXY(tmp_result, 0.0);
 end;
 chrGrafica.AddSeries( TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) );
 Result.ResFloat:=tmp_result;
end;

procedure TWMainForm.intersect_lineal_solver(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  solver_linear: SolverEq;
  x0: Real;
  method: Integer;
  tmp_result: Real;
  fx_to_solve, fx1,fx2: String;
  dyn_calc, dyn_calc_2:  FunctionDynamicClass;
begin
 fx1 := Args[0].ResString;
 fx2 := Args[1].ResString;
 x0 := ArgToFloat(Args[2]);

 fx_to_solve:= fx1 + '-(' + fx2 + ')';
 solver_linear:=SolverEq.create;
 solver_linear.fx:=fx_to_solve;
 solver_linear.solve_a:=0;
 solver_linear.solve_b:=x0;
 solver_linear.solve_h:=0.00001;
 solver_linear.ErrorAllowed:=0.0001;
 solver_linear.MethodType:=2;

 tmp_result:=solver_linear.execute();

 dyn_calc:=FunctionDynamicClass.Create(fx1);
 FunctionList.Add( TFuncSeries.Create( chrGrafica ) );
 with TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'FuncSeries' + IntToStr( FunctionList.Count );
      Pen.Color:= colorbtnFunction1.ButtonColor;
      Pen.Width:= 1;
      Tag:=0;
      Active:= True;
      OnCalculate:=@dyn_calc.calculate_function;
 end;
 chrGrafica.AddSeries( TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) );

 dyn_calc_2:=FunctionDynamicClass.Create(fx2);
 FunctionList.Add( TFuncSeries.Create( chrGrafica ) );
 with TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'FuncSeries' + IntToStr( FunctionList.Count );
      Pen.Color:= colorbtnFunction1.ButtonColor;
      Pen.Width:= 1;
      Tag:=0;
      Active:= True;
      OnCalculate:=@dyn_calc_2.calculate_function;
 end;

 chrGrafica.AddSeries( TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) );

 FunctionList.Add( TLineSeries.Create( chrGrafica ) );
 with TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'LineSeries' + IntToStr( FunctionList.Count );
      LinePen.Color:= clRed;
      LinePen.Width:= 1;
      LinePen.Style:=psDot;
      Pointer.Style:=psCircle;
      Pointer.Brush.Color:=clRed;
      Tag:=0;
      Active:= True;
      ShowLines:=False;
      ShowPoints:=True;
      AddXY(tmp_result, 0.0);
 end;
 chrGrafica.AddSeries( TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) );
 Result.ResFloat:=tmp_result;
end;

procedure TWMainForm.solver_area(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  solver_integral: IntegralSolver;
  graph: TfrmGraficadora;
  a,b: Double;
  n, method: Integer;
  fx: String;
  it_integral: Real;
  parse_area: TParseMath;
  dyn_calc: FunctionDynamicClass;
begin

 fx := Args[0].ResString;
 a := ArgToFloat(Args[1]);
 b := ArgToFloat(Args[2]);
 n := Args[3].ResInteger;
 method := Args[4].ResInteger;

 parse_area:=TParseMath.create();
 parse_area.AddVariable('x',0.0);
 solver_integral := IntegralSolver.create;
 solver_integral.is_area:=True;
 case method of
  0: begin
   Result.ResFloat:= solver_integral.trapezoidal_method(n,a,b,fx);
  end;
  1: begin
   Result.ResFloat:= solver_integral.simpson_method(n,a,b,fx);
  end;
  2: begin
   Result.ResFloat:= solver_integral.simpson_method_3_8(n,a,b,fx);
  end;
 end;


 parser_functions.Expression:=fx;
 parse_area.Expression:=fx;
 FunctionList.Add( TFuncSeries.Create( chrGrafica ) );
 dyn_calc:=FunctionDynamicClass.Create(fx);
 with TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'FuncSeries' + IntToStr( FunctionList.Count );
      Pen.Color:= colorbtnFunction1.ButtonColor;
      Pen.Width:= 1;
      Tag:=0;
      Active:= True;
      OnCalculate:=@dyn_calc.calculate_function;
 end;
 chrGrafica.AddSeries( TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) );
 FunctionList.Add( TAreaSeries.Create( chrGrafica ) );
 with TAreaSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'FuncSeries' + IntToStr( FunctionList.Count );
      AreaBrush.Color:=$00E1E1FF;
      AreaContourPen.Color:=clRed;
      AreaContourPen.Cosmetic:=True;
      AreaContourPen.Style:=psDot;
      AreaLinesPen.Style:=psClear;
      AxisIndexX:=0;
      Tag:=0;
      Active:= True;
      Transparency:=150;
      Stacked:=True;
      it_integral:=a;
      UseZeroLevel:=True;
      while it_integral<=b do begin
           parse_area.NewValue('x',it_integral);
           AddXY(it_integral,parse_area.Evaluate());
           it_integral:=it_integral+solver_integral.local_h;
      end;
 end;
 chrGrafica.AddSeries( TAreaSeries( FunctionList[ FunctionList.Count - 1 ] ) );




end;

procedure simpson_method(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  solver_integral: IntegralSolver;
  graph: TfrmGraficadora;
  a,b: Double;
  n: Integer;
  fx: String;
begin
 fx := Args[0].ResString;
 a := ArgToFloat(Args[1]);
 b := ArgToFloat(Args[2]);
 n := Args[3].ResInteger;
 solver_integral := IntegralSolver.create;
 Result.ResFloat:= solver_integral.simpson_method_3_8(n,a,b,fx);
end;

procedure TWMainForm.diferential_solver(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var fxy: String;
  xn, end_xn, h, f_xn: Real;
  dsolver: DiferentialSolver;
  method: Integer;
  i: Integer;
  x_values, y_values: TList;
  line_serie: TLineSeries;
begin
 fxy := Args[0].ResString;
 xn := ArgToFloat(Args[1]);
 end_xn := ArgToFloat(Args[2]);
 f_xn := ArgToFloat(Args[3]);
 method := Args[4].ResInteger;
 h := ArgToFloat(Args[5]);
 dsolver := DiferentialSolver.create;
 if method = 1 then
 begin
  Result.ResFloat:= dsolver.simple_euler(xn,end_xn,h,f_xn, fxy);
 end
 else if method = 2 then
 begin
  Result.ResFloat:= dsolver.optimized_euler(xn,end_xn,h,f_xn, fxy);
 end
 else if method = 3 then
 begin
  Result.ResFloat:= dsolver.runge_kutta(xn,end_xn,h,f_xn, fxy);
 end
 else if method = 4 then
 begin
  Result.ResFloat:= dsolver.dormand_prince(xn,end_xn,h,f_xn, fxy);
 end
 else begin
  Result.ResFloat:=0;
  Exit;
 end;
 x_values:= dsolver.x_results;
 y_values:= dsolver.y_results;
 FunctionList.Add( TLineSeries.Create( chrGrafica ) );
 with TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
      Name:= 'LineSeries' + IntToStr( FunctionList.Count );
      LinePen.Color:= colorbtnFunction1.ButtonColor;
      LinePen.Width:= 1;
      Tag:=0;
      Active:= True;
      for i:=0 to x_values.Count-1 do
      begin
           AddXY(real(x_values[i]), real(y_values[i]));
      end;
 end;
 chrGrafica.AddSeries( TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) );


end;

procedure TWMainForm.trbMax1Change(Sender: TObject);
begin
  chrGrafica.Extent.XMax := trbMax1.Position;
end;

procedure TWMainForm.trbMin1Change(Sender: TObject);
begin
  chrGrafica.Extent.XMin := trbMin1.Position;
end;

function OccurrencesOfChar(const S: string; const C: char): integer;
var
  i: Integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      inc(result);
end;

procedure TWMainForm.ReaderTimerTimer(Sender: TObject);
begin
end;

procedure TWMainForm.FormCreate(Sender: TObject);
begin

 DoubleBuffered := True;
 DText          := TStringList.Create;
 if FileExists(Dir+'/demotext.txt') then DText.LoadFromFile(Dir+'/demotext.txt');
 CmdBox.StartRead(clSilver,clNavy,'/example/prompt/>',clYellow,clNavy);
 CmdBox.TextColors(clWhite,clNavy);
 CmdBox.Writeln(#27#218#27#10#191);
 CmdBox.Writeln(#27#179'Type "help" to see a short list of available commands.'#27#10#179);
 CmdBox.Writeln(#27#217#27#10#217);
 FParser := TFPExpressionParser.Create(nil);
 FParser.Identifiers.AddFunction('integral', 'F', 'SFFII', @integral);
 FParser.Identifiers.AddFunction('area', 'F', 'SFFII', @solver_area);
 FParser.Identifiers.AddFunction('edo', 'F', 'SFFFIF', @diferential_solver);
 FParser.Identifiers.AddFunction('plot2d', 'F', 'SFF', @graph_function);
 FParser.Identifiers.AddFunction('raiz', 'F', 'SFFI', @lineal_solver);
 FParser.Identifiers.AddFunction('intersect', 'F', 'SSF', @lineal_solver);



 chrGrafica.Extent.UseXMax:= true;
 chrGrafica.Extent.UseXMin:= true;
 Memory:= TMClassMap.Create;
 Memory_Arrays:= TArrayClassMap.Create;
 Memory_Matrix:= TMatrixClassMap.Create;
 FunctionList:= TList.Create;
 functions_list_to_calc:=TStringList.Create;
 parser_functions:=TParseMath.create();
 parser_functions.AddVariable('x',0.0);
end;

function TWMainForm.get_from_memory(key: String; VAR exist: Boolean; VAR type_str: String): String;
var index: Integer;
  matrix_tmp: TMatrix;
begin
 Result:= 'None';
 exist:=False;
 if Memory.IndexOf(key)<>-1 then
 begin
  type_str:= Memory[key].m_type;
  Result := Memory[key].value;
  exist:=True;
 end;
end;

procedure TWMainForm.resolve_function(input: String; VAR result_resolve : String);
var re_param: TRegExpr;
  re_real: TRegExpr;
  re_string: TRegExpr;
  re_get, re_matrix: TRegExpr;
  function_name, function_fixed: String;
  parameters, splited_params: TStringList;
  typed_parameters: TList;
  tmp_parameter, tmp_key: String;
  from_memory_str: String;
  exist, success: Boolean;
  result_method, it_lagrange: Real;
  matrix_tmp: TMatrix;
  array_str_tmp, array_variables: TStringList;
  type_str: String;
  matrix_ptr    : PTMatrix;
  general_solver_eq: GeneralSolverEq;
  lagrange_solver_pol: LagrangePol;
  pointer_matrix: Pointer;
  dyn_func: FunctionDynamicClass;
  i: Integer;
begin
 re_param:= TRegExpr.Create('[^,(?!)]+');
 parameters:= TStringList.Create;
 typed_parameters:= TList.Create;
 splited_params:= TStringList.Create;
 re_real := TRegExpr.Create('^[-]?([0-9]+[.]+)?[0-9]+$');
 re_string := TRegExpr.Create('''(.*?)''');
 re_get := TRegExpr.Create('^[_a-zA-Z]+[_a-zA-Z0-9]*$');
 re_matrix := TRegExpr.Create('^\[(.*?)\]$');
 matrix_tmp := TMatrix.Create(1,1);

 if re_param.Exec(input) then begin
  function_name:= re_param.Match[0];
  splited_params.LineBreak := function_name;
  splited_params.Text := input;
  tmp_parameter := copy(splited_params[1],2,splited_params[1].Length-2);
  splited_params.Clear;
  ExtractStrings([','],[],PChar(tmp_parameter),splited_params);

  for i:=0 to splited_params.Count-1 do begin
      tmp_parameter:= splited_params[i];
      if (check_valid_constant(tmp_parameter)) then begin
       parameters.Add(tmp_parameter);
       CmdBox.Writeln('is a constant '+ tmp_parameter);
      end
      else if re_matrix.Exec(tmp_parameter) and resolve_matrix_or_array_content('key_tmp',tmp_parameter,pointer_matrix,type_str) then begin
       parameters.Add(tmp_parameter);
       case type_str of
        'matrix':
          begin
           typed_parameters.Add(Memory_Matrix['key_tmp']);
          end;
        'array':
          begin
           typed_parameters.Add(Memory_Arrays['key_tmp']);
          end;
       end;
       CmdBox.Writeln('is a matrix or array constant '+ tmp_parameter);
      end
      else if re_get.Exec(tmp_parameter) then begin
       from_memory_str:= get_from_memory(tmp_parameter, exist, type_str);
       if not exist then begin
        CmdBox.Writeln('Variable ' + tmp_parameter + ' not registered');
        Exit;
       end;
       parameters.Add(from_memory_str);
       case type_str of
        'matrix':
          begin
           typed_parameters.Add(Memory_Matrix[tmp_parameter]);
          end;
        'array':
          begin
           typed_parameters.Add(Memory_Arrays[tmp_parameter]);
          end;
       end;

      end;
  end;
 end;
 //TODO: method and h needs be managed in both cases
 case function_name of
  'edo':
    begin
     if parameters.Count < 4 then begin
      CmdBox.Writeln('Not Enough Parameters to edo');
      Exit;
     end
     else if parameters.Count = 4 then begin
      parameters.Add('4');
      parameters.Add('0.01');
     end
     else if parameters.Count = 5 then begin
      parameters.Add('0.01');
     end;
    end;
  'determinant':
    begin
     if parameters.Count <> 1 then begin
      CmdBox.Writeln('Not Enough Parameters to Determinant');
      Exit;
     end;
     matrix_tmp := TMatrix(typed_parameters[0]);
     result_resolve:=FloatToStr(matrix_tmp.MDeterminant(matrix_tmp));
     CmdBox.Writeln(result_resolve);
     Exit;
    end;
  'inverse':
    begin
     if parameters.Count <> 1 then begin
      CmdBox.Writeln('Not Enough Parameters to Inverse');
      Exit;
     end;
     matrix_tmp := TMatrix(typed_parameters[0]);
     matrix_tmp := matrix_tmp.MInverse(matrix_tmp);
     result_resolve:=matrix_tmp.To_reuse_String();
     CmdBox.Writeln(matrix_tmp.To_String());
     Exit;
    end;
  'senl':
    begin
     if (parameters.Count <> 2) or (typed_parameters.Count <> 2) then begin
      CmdBox.Writeln('Not Enough or Bad Parameters to SENL');
      Exit;
     end;
     general_solver_eq := GeneralSolverEq.create;
     array_str_tmp:= TStringList(typed_parameters[0]);
     matrix_tmp:= TMatrix(typed_parameters[1]);
     array_variables:=TStringList.Create;
     if array_str_tmp.Count = 2 then begin
      WriteLn('Here in two');
      array_variables.Add('x');
      array_variables.Add('y');
     end
     else if array_str_tmp.Count = 3 then begin
      array_variables.Add('x');
      array_variables.Add('y');
      array_variables.Add('z');
     end
     else if array_str_tmp.Count = 4 then begin
      array_variables.Add('x');
      array_variables.Add('y');
      array_variables.Add('z');
      array_variables.Add('w');
     end
     else if array_str_tmp.Count = 5 then begin
      array_variables.Add('x');
      array_variables.Add('y');
      array_variables.Add('z');
      array_variables.Add('w');
      array_variables.Add('v');
     end
     else begin
      CmdBox.Writeln('Not enough or bad number of functions');
      Exit;
     end;
     general_solver_eq.read_from_string_matrix(array_str_tmp, array_variables, matrix_tmp, success);
     general_solver_eq.ErrorAllowed:= 0.0001;
     general_solver_eq.H_variable:= 0.00001;
     result_resolve:=general_solver_eq.newthon_rapsody().To_reuse_String();
     CmdBox.Writeln(result_resolve);
     Exit;
    end;
  'lagrange':
    begin
     if (parameters.Count <> 1) or (typed_parameters.Count <> 1) then begin
      CmdBox.Writeln('Not Enough or Bad Parameters to Lagrange');
      Exit;
     end;
     matrix_tmp:= TMatrix(typed_parameters[0]);
     if (Length(matrix_tmp.A[0]) <> 2) then begin
      CmdBox.Writeln('Bad Dimension of Matrix to Lagrange');
      Exit;
     end;
     lagrange_solver_pol:= LagrangePol.Create;
     lagrange_solver_pol.read_points_from_matrix(matrix_tmp);
     result_resolve:=''''+lagrange_solver_pol.generate_function()+'''';
     CmdBox.Writeln(result_resolve);

     FunctionList.Add( TLineSeries.Create( chrGrafica ) );
     with TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
          Name:= 'LineSeries' + IntToStr( FunctionList.Count );
          LinePen.Color:= colorbtnFunction1.ButtonColor;
          LinePen.Width:= 1;
          Tag:=0;
          Active:= True;
          ShowLines:=True;
          it_lagrange:=lagrange_solver_pol.min_x;
          while it_lagrange <= lagrange_solver_pol.max_x do
          begin
           AddXY(it_lagrange,lagrange_solver_pol.eval_generated_function(it_lagrange).V);
           it_lagrange:=it_lagrange+0.01;
          end;
          //for i:=0 to Length(lagrange_solver_pol.x_points)-1 do
          //begin
          // AddXY(lagrange_solver_pol.x_points[i], lagrange_solver_pol.y_points[i]);
          //end;
     end;
     chrGrafica.AddSeries( TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) );

     FunctionList.Add( TLineSeries.Create( chrGrafica ) );
     with TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
          Name:= 'LineSeries' + IntToStr( FunctionList.Count );
          LinePen.Color:= clRed;
          LinePen.Width:= 1;
          LinePen.Style:=psDot;
          Tag:=0;
          Active:= True;
          ShowLines:=False;
          ShowPoints:=True;
          for i:=0 to Length(lagrange_solver_pol.x_points)-1 do
          begin
           AddXY(lagrange_solver_pol.x_points[i], lagrange_solver_pol.y_points[i], '',clRed);
          end;
     end;
     chrGrafica.AddSeries( TLineSeries( FunctionList[ FunctionList.Count - 1 ] ) );
     Exit;
     Exit;
    end;
  'integral':
    begin
     if (parameters.Count < 1) or (parameters.Count > 5) or (typed_parameters.Count > 1) then begin
      CmdBox.Writeln('Not Enough or Bad Parameters to SENL');
      Exit;
     end
     else if(parameters.Count = 3) then begin
      CmdBox.Writeln('Using default n');
      parameters.Add('100');
      parameters.Add('2');
     end
     else if(parameters.Count = 4) then begin
      CmdBox.Writeln('Using default n');
      parameters.Add('2');
     end;
    end;
  'area':
    begin
     if (parameters.Count < 1) or (parameters.Count > 5) or (typed_parameters.Count > 1) then begin
      CmdBox.Writeln('Not Enough or Bad Parameters to SENL');
      Exit;
     end
     else if(parameters.Count = 3) then begin
      CmdBox.Writeln('Using default n and method');
      parameters.Add('100');
      parameters.Add('1');
     end
     else if(parameters.Count = 4) then begin
      CmdBox.Writeln('Using default method');
      parameters.Add('1');
     end;
    end;
  'plot2d':
    begin
     if (parameters.Count = 1) and (typed_parameters.Count < 1) then begin
      dyn_func:=FunctionDynamicClass.Create(copy(parameters[0],2,parameters[0].Length-2));
      FunctionList.Add( TFuncSeries.Create( chrGrafica ) );
      with TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) do begin
           Name:= 'FuncSeries' + IntToStr( FunctionList.Count );
           Pen.Color:= colorbtnFunction1.ButtonColor;
           Pen.Width:= 1;
           Tag:=0;
           Active:= True;
           OnCalculate:=@dyn_func.calculate_function;
      end;
      chrGrafica.AddSeries( TFuncSeries( FunctionList[ FunctionList.Count - 1 ] ) );
      Exit;
     end
     else if (parameters.Count < 3) or (typed_parameters.Count > 1) then begin
      CmdBox.Writeln('Not Enough or Bad Parameters to plot2d');
      Exit;
     end
    end;
  'raiz':
    begin
     CmdBox.Writeln('raiz method');
     if (parameters.Count < 3) or (typed_parameters.Count > 0) then begin
      CmdBox.Writeln('Not Enough or Bad Parameters to Raiz');
      Exit;
     end
     else if (parameters.Count = 3) then begin
       parameters.Add('2');
     end;
    end;
  'intersect':
    begin
      if (parameters.Count < 3) or (typed_parameters.Count > 0) then begin
       CmdBox.Writeln('Not Enough or Bad Parameters to Intersect');
       Exit;
      end
    end
  else
    begin
     CmdBox.Writeln('Unknown Function');
     Exit;
    end;
 end;
 CmdBox.Writeln('Resolving ' + function_name);

 function_fixed:= function_name+'('+parameters[0];
 for i:=1 to parameters.Count-1 do begin
  function_fixed:=function_fixed+','+parameters[i];
 end;
 function_fixed:= function_fixed+')';
 CmdBox.Writeln(function_fixed);
 try
   FParser.Expression := function_fixed;
   result_method:= FParser.Evaluate.ResFloat;
   CmdBox.Writeln(FloatToStr(result_method));
   result_resolve:=FloatToStr(result_method);
 except
   CmdBox.Writeln('Bad Arguments');
 end;
end;

function TWMainForm.resolve_matrix_or_array_content(key,value: String; VAR ret_pointer: Pointer; VAR type_ret: String): Boolean;
var re, re_get, re_string, re_matrix: TRegExpr;
  tmp_pair_key_value: PairValueType;
  matrix: TMatrix;
  elements, ret_elements: TStringList;
  tmp_match, tmp_element, type_str: String;
  exist: Boolean;
  j: Integer;
begin
 Result:= True;
 tmp_match:= value;
 matrix := TMatrix.Create(1,1);
 if matrix.read_from_string(tmp_match, matrix) then
 begin
  tmp_pair_key_value.m_type:= 'matrix';
  tmp_pair_key_value.value:= tmp_match;
  ret_pointer:= matrix;
  type_ret:='matrix';
  Memory[key] := tmp_pair_key_value;
  Memory_Matrix[key] := matrix;
 end
 else if not AnsiContainsStr(tmp_match, ';') then
 begin
  elements := TStringList.Create;
  tmp_match:= Copy(tmp_match,2,Length(tmp_match)-2);
  ExtractStrings([' '],[],PChar(tmp_match),elements);
  ret_elements := TStringList.Create;
  for j:=0 to elements.Count-1 do
  begin
       tmp_element := StringReplace(elements[j],' ','', [rfReplaceAll]);
       if check_valid_constant(tmp_element) then
       begin
        ret_elements.Add(tmp_element);
       end
       else
       begin
        tmp_match:=get_from_memory(tmp_element, exist, type_str);
        if exist then begin
         ret_elements.Add(tmp_match);
        end
        else begin
         CmdBox.Writeln('Variable '+tmp_element+' not registered');
         Result:=False;
         Exit;
        end;
       end;
  end;
  tmp_match:= '[';
  ret_elements.Delimiter := ' ';
  tmp_match:= tmp_match + ret_elements.DelimitedText + ']';
  tmp_pair_key_value.m_type:= 'array';
  tmp_pair_key_value.value:= tmp_match;
  type_ret:='array';
  ret_pointer:=ret_elements;
  Memory[key] := tmp_pair_key_value;
  Memory_Arrays[key] := ret_elements;
 end
 else
 begin
  CmdBox.Writeln('Invalid Matrix');
  Result:= False;
  Exit;
 end;
end;

function TWMainForm.register_in_memory(key, value: String): Boolean;
var re, re_get, re_string, re_matrix: TRegExpr;
  tmp_match, tmp_element: String;
  matrix: TMatrix;
  j: Integer;
  exist: Boolean;
  elements, ret_elements: TStringList;
  tmp_pair_key_value: PairValueType;
  ret_type: String;
  ret_pointer: Pointer;
begin
 Result:= True;
 re_get := TRegExpr.Create('^[_a-zA-Z]+[_a-zA-Z0-9]*$');
 re := TRegExpr.Create('^[-]?([0-9]+[.]+)?[0-9]+$');
 re_string := TRegExpr.Create('^''(.*?)''$');
 re_matrix := TRegExpr.Create('^\[(.*?)\]$');
 matrix := TMatrix.Create(1,1);
 if re.Exec(value) then
 begin
   tmp_match := re.Match[0];
   tmp_pair_key_value.m_type:= 'real';
   tmp_pair_key_value.value:= tmp_match;
   Memory[key] := tmp_pair_key_value;
   CmdBox.Writeln(tmp_match + ' is real');
 end
 else if re_string.Exec(value) then
 begin
   tmp_match := re_string.Match[0];
   tmp_pair_key_value.m_type:= 'function';
   tmp_pair_key_value.value:= tmp_match;
   Memory[key] := tmp_pair_key_value;
   CmdBox.Writeln(tmp_match + ' is function');
 end
 else if (re_matrix.Exec(value) and resolve_matrix_or_array_content(key,value,ret_pointer,ret_type)) then
 begin
  CmdBox.Writeln('Array or Matrix Accepted');
 end
 else
 begin
  CmdBox.Writeln('Uknown Type of data to input');
  Result:=False;
  Exit;
 end;
end;


function TWMainForm.check_for_memory(input: String): Boolean;
var  re: TRegExpr;
  re_get, re_function: TRegExpr;
  aList: TStringList;
  tmp_match, resolve_result: String;
  exist: Boolean;
  type_str: String;
begin
 Result:=True;
 re_function := TRegExpr.Create('^[_a-zA-Z]+[_a-zA-Z0-9]*\({1}\S+\)$');
 //re := TRegExpr.Create('[a-z]+[^=]*={1}\S+');
 re := TRegExpr.Create('^[_a-zA-Z]+[_a-zA-Z0-9]*={1}.+$');
 re_get := TRegExpr.Create('^[_a-zA-Z]+[_a-zA-Z0-9]*$');
 aList:= TStringList.Create;
 if re.Exec(input) then
 begin
   tmp_match := re.Match[0];
   aList.Delimiter:= '=';
   aList.StrictDelimiter:= True;
   aList.DelimitedText:=tmp_match;
   if re_function.Exec(aList[1]) then
   begin
      resolve_function(aList[1], resolve_result);
      register_in_memory(aList[0], resolve_result);
   end
   else begin
    register_in_memory(aList[0], aList[1]);
   end;
 end
 else if re_get.Exec(input) then
 begin
   tmp_match:=get_from_memory(input, exist, type_str);
   if exist then begin
     CmdBox.Writeln(tmp_match);
   end
   else begin
    CmdBox.Writeln('Variable not assigned');
   end;
 end
 else
 begin
  Result:=False;
 end;
 re.Free;
end;

procedure TWMainForm.CmdBoxInput(ACmdBox: TCmdBox; Input: String);
var i:Integer;
 result_method: Float;
 re_function: TRegExpr;
 resolve_result: String;
begin
 re_function := TRegExpr.Create('^[_a-zA-Z]+[_a-zA-Z0-9]*\({1}.+\)$');
 if rdpw then
 begin
  CmdBox.TextColors(clLime,clBlue);
  CmdBox.Writeln('Your Secret Password : '+Input);
  CmdBox.TextColors(clSilver,clNavy);
  rdpw:=false;
 end
 else
 begin
  rdpw:=false;
  Input:=LowerCase(Input);
  if Input='help' then
  begin
   CmdBox.TextColors(clLime,clNavy);
   CmdBox.Writeln(#27#218#27#197#128#0#27#194#27#10#191);
   CmdBox.Writeln(#27#179' Command'#27#33#128#0#27#179' Explanation'#27#10#179);
   CmdBox.Writeln(#27#195#27#197#128#0#27#198#27#10#180);
   CmdBox.Writeln(#27#179' help'#27#33#128#0#27#179' Gives this list of Commands'#27#10#179);
   CmdBox.Writeln(#27#179' clear'#27#33#128#0#27#179' Clears the Content of CmdBox'#27#10#179);
   CmdBox.Writeln(#27#179' start'#27#33#128#0#27#179' Outputs the Content of Demotext.txt from the beginning'#27#10#179);
   CmdBox.Writeln(#27#179' stop'#27#33#128#0#27#179' Stops output and resets to Start'#27#10#179);
   CmdBox.Writeln(#27#179' pause'#27#33#128#0#27#179' Interrupts output'#27#10#179);
   CmdBox.Writeln(#27#179' resume'#27#33#128#0#27#179' Resumes output from the last position'#27#10#179);
   CmdBox.Writeln(#27#179' clearhistory'#27#33#128#0#27#179' Clears all history entries'#27#10#179);
   CmdBox.Writeln(#27#179' readpwd'#27#33#128#0#27#179' Read a Password (just as a test)'#27#10#179);
   CmdBox.Writeln(#27#179' exit'#27#33#128#0#27#179' Exit program'#27#10#179);
   CmdBox.Writeln(#27#217#27#197#128#0#27#193#27#10#217);
   CmdBox.TextColor(clSilver);
  end else
  if Input='readpwd' then
  begin
   rdpw:=true;
  end else
  if Input='clearhistory' then
  begin
   CmdBox.TextColor(clYellow);
   CmdBox.Writeln('Clear History...');
   CmdBox.TextColor(clSilver);
   CmdBox.ClearHistory;
  end else
  if Input='start' then
  begin
   TextPosition:=0;
   ReaderTimer.Enabled:=true;
   CmdBox.TextColors(clLime,clBlue);
   CmdBox.Writeln('Start...');
  end else if Input='stop' then
  begin
   TextPosition:=0;
   ReaderTimer.Enabled:=false;
   CmdBox.TextColors(clRed,clBlue);
   CmdBox.Writeln('Stop...');
  end else if Input='pause' then
  begin
   ReaderTimer.Enabled:=false;
   CmdBox.TextColors(clPurple,clBlue);
   CmdBox.Writeln('Pause...');
  end else if Input='resume' then
  begin
   ReaderTimer.Enabled:=true;
   CmdBox.TextColors(clGreen,clBlue);
   CmdBox.Writeln('Continue...');
  end
  //else if pos('dsolve', Input)<>0 then
  //begin
  //   if OccurrencesOfChar(Input,',') = 4 then
  //   begin
  //      Input:= copy(Input,0,Length(Input)-1) + ',''euler'')';
  //   end;
  //
  //   FParser.Expression := Input;
  //   result_method:= FParser.Evaluate.ResFloat;
  //   CmdBox.TextColors(clGreen,clBlue);
  //   CmdBox.Writeln(FloatToStr(result_method));
  //
  //end
  else if check_for_memory(Input) then
  begin
     CmdBox.Writeln('Memory Accessed');
  end
  else if re_function.Exec(Input) then
  begin
   CmdBox.Writeln('Resolving function');
   resolve_function(Input, resolve_result);
  end
  else if Input='clear_chart()' then
  begin
   for i := chrGrafica.Series.Count-1 downto 2 do begin
    chrGrafica.Series[i].Free;
   end;
   FunctionList.Clear;
  end
  else if Input='clear' then
  begin
   CmdBox.Clear;
  end else if Input='exit' then close else
  begin
   CmdBox.Writeln('Invalid Command!');
  end;
 end;
 if rdpw then CmdBox.StartReadPassWord(clYellow,clNavy,'Pwd:',clLime,clNavy) else
 CmdBox.StartRead(clSilver,clNavy,'/example/prompt/>',clYellow,clNavy);
 HistoryList.Clear;
 for i:=0 to CmdBox.HistoryCount-1 do HistoryList.Items.Add(CmdBox.History[i]);
end;

procedure TWMainForm.CbSetCaretChange(Sender: TObject);
begin
 case cbSetCaret.ItemIndex of
  0:CmdBox.CaretType := cartLine;
  1:CmdBox.CaretType := cartSubBar;
  2:CmdBox.CaretType := cartBigBar;
 end;
 CmdBox.SetFocus;
end;

procedure TWMainForm.Button2Click(Sender: TObject);
begin
 CmdBox.ClearHistory;
 HistoryList.Clear;
end;

procedure TWMainForm.Button3Click(Sender: TObject);
begin
 FProcess:=0;
 ProcessTimer.Enabled:=True;
end;

procedure TWMainForm.Button4Click(Sender: TObject);
begin
  FontDialog.Font:=CmdBox.Font;
  if FontDialog.Execute then
  begin
    CmdBox.Font:=FontDialog.Font;
  end;
end;

procedure TWMainForm.cbWordWrapChange(Sender: TObject);
begin
  if CmdBox.WrapMode=wwmWord then CmdBox.WrapMode:=wwmChar else CmdBox.WrapMode:=wwmWord;
end;

procedure TWMainForm.chkProportional1Change(Sender: TObject);
begin
  chrGrafica.Proportional:= not chrGrafica.Proportional;
end;

procedure TWMainForm.Button1Click(Sender: TObject);
begin
 Close;
end;

procedure TWMainForm.FormDestroy(Sender: TObject);
begin
 DText.Free;
end;

procedure TWMainForm.OperationPanelClick(Sender: TObject);
begin

end;

procedure TWMainForm.ProcessTimerTimer(Sender: TObject);
begin
 if FProcess=100 then
 begin
  CmdBox.ClearLine;
  ProcessTimer.Enabled:=False;
 end
 else
 begin
  CmdBox.TextColors(clRed,clBlue);
  CmdBox.Write('Processing ['+IntToStr(FProcess)+'%]'#13);
 end;
 Inc(FProcess);
end;

initialization
  {$I wnmainform.lrs}
  Dir:=ExtractFileDir(ParamStr(0));
end.

