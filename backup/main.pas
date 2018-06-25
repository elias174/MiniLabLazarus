unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TAFuncSeries, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, ColorBox, ParseMath, TAChartUtils,
  TASources, TATools  , TACustomSeries;

type

  { TfrmGraficadora }

  TfrmGraficadora = class(TForm)
    btnGraficar: TButton;
    Button1: TButton;
    chkEscogerN: TCheckBox;
    chkMostrarPuntos: TCheckBox;
    chkMostrarValores: TCheckBox;
    chkUsarPloteo: TCheckBox;
    chrGrafica: TChart;
    cboFuncion: TComboBox;
    Area: TAreaSeries;
    ediN: TEdit;
    AreaA: TEdit;
    AreaB: TEdit;
    Funcion: TFuncSeries;
    Label3: TLabel;
    pnlOpciones: TPanel;
    Plotear: TLineSeries;
    cboxColorFuncion: TColorBox;
    ediIntervalo: TEdit;
    EjeX: TConstantLine;
    EjeY: TConstantLine;
    Label1: TLabel;
    Label2: TLabel;
    lblFuncion: TLabel;
    pnlContenedor: TPanel;
    procedure AreaGetMark(out AFormattedMark: String; AIndex: Integer);
    procedure btnGraficarClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cboxColorFuncionChange(Sender: TObject);
    procedure chkUsarPloteoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FuncionCalculate(const AX: Double; out AY: Double);

    Procedure GraficarFuncion();
    Procedure GraficarFuncionConPloteo();

  private
    { private declarations }
    Parse  : TparseMath;
    Xminimo,
    Xmaximo: String;

    function f( x: Double ): Double;
    Procedure DetectarIntervalo();

  public
    { public declarations }
  end;

var
  frmGraficadora: TfrmGraficadora;


implementation

{$R *.lfm}

{ TfrmGraficadora }

Procedure TfrmGraficadora.DetectarIntervalo();
var
    PosCorcheteIni, PosCorcheteFin, PosSeparador: Integer;
    PosicionValidad: Boolean;
    i: Integer;
    x: Double;
const
   CorcheteIni = '[';
   CorcheteFin = ']';
   Separador = ';';

begin

 PosCorcheteIni:= Pos( CorcheteIni, ediIntervalo.Text );
 PosCorcheteFin:= pos( CorcheteFin, ediIntervalo.Text );
 PosSeparador:= Pos( Separador, ediIntervalo.Text  );

 PosicionValidad:= ( PosCorcheteIni > 0);
 PosicionValidad:= PosicionValidad and ( PosSeparador > 2);
 PosicionValidad:= PosicionValidad and ( PosCorcheteFin > 3);

 if not PosicionValidad then begin
        ShowMessage( 'Error en el intervalo');
        exit;

 end;

  Xminimo:= Copy( ediIntervalo.Text,
                      PosCorcheteIni + 1,
                      PosSeparador - 2 );

  Xminimo:= Trim( Xminimo );
  Xmaximo:= Copy( ediIntervalo.Text,
                      PosSeparador + 1,
                      Length( ediIntervalo.Text ) - PosSeparador -1  );

 Xmaximo:= Trim( Xmaximo );

end;

function TfrmGraficadora.f( x: Double ): Double;
begin
     parse.Expression:= cboFuncion.Text;
     Parse.NewValue('x' , x );
     Result:= Parse.Evaluate();
end;


Procedure TfrmGraficadora.GraficarFuncion();
begin
    Plotear.Clear;
    with Funcion do begin
      Active:= False;


      Extent.XMax:= StrToFloat( Xmaximo );
      Extent.XMin:= StrToFloat( Xminimo );

      Extent.UseXMax:= true;
      Extent.UseXMin:= true;
      Funcion.Pen.Color:=  cboxColorFuncion.Colors[ cboxColorFuncion.ItemIndex ];

      Active:= True;

  end;
end;

Procedure TfrmGraficadora.GraficarFuncionConPloteo();
var i: Integer;
    Max, Min, h, NewX, NewY: Real;
    IsInIntervalArea: Boolean;
begin
    Funcion.Active:= False;
    Area.Active:= False;

    Max:=  StrToFloat( Xmaximo );
    Min:=  StrToFloat( Xminimo ) ;

    if chkEscogerN.Checked then
       h:= 1 / StrToFloat( ediN.Text )

    else
        h:= abs( ( Max - Min )/( 100 * Max ) );

    if chkMostrarValores.Checked then
       Plotear.Marks.Style:= smsValue
    else
       Plotear.Marks.Style:= smsNone;

    Plotear.ShowPoints:= chkMostrarPuntos.Checked;

    NewX:= StrToFloat( Xminimo );
    Plotear.LinePen.Color:= cboxColorFuncion.Colors[ cboxColorFuncion.ItemIndex ];  ;

    while NewX < Max do begin
       NewY:= f( NewX );
       Plotear.AddXY( NewX, NewY );
       IsInIntervalArea:= ( NewX >= StrToFloat( AreaA.Text ) ) and (  NewX <= StrToFloat( AreaB.Text )) ;
       if IsInIntervalArea then
          Area.AddXY( NewX, NewY );
       NewX:= NewX + h;

    end;

    Plotear.Active:= true;

end;

procedure TfrmGraficadora.btnGraficarClick(Sender: TObject);
begin

 Area.Clear;
 Plotear.Clear;

 if Trim( cboFuncion.Text ) = '' then
    exit;

 DetectarIntervalo();

 if chkUsarPloteo.Checked then
    GraficarFuncionConPloteo()

 else
    GraficarFuncion();

end;

procedure TfrmGraficadora.AreaGetMark(out AFormattedMark: String;
  AIndex: Integer);
begin

end;

procedure TfrmGraficadora.Button1Click(Sender: TObject);
begin

 Area.Active:= true;

end;

procedure TfrmGraficadora.cboxColorFuncionChange(Sender: TObject);
begin

end;

procedure TfrmGraficadora.chkUsarPloteoChange(Sender: TObject);
begin
  pnlOpciones.Enabled:= chkUsarPloteo.Checked;
end;

procedure TfrmGraficadora.FormCreate(Sender: TObject);
begin
  Parse := TParseMath.create;
  parse.AddVariable('x',0.0);
  //Parse.AddVariable('y',0.0);
end;

procedure TfrmGraficadora.FormDestroy(Sender: TObject);
begin
  Parse.destroy;
end;

procedure TfrmGraficadora.FuncionCalculate(const AX: Double; out AY: Double);

begin
  AY := f( AX ) ;

end;




end.

