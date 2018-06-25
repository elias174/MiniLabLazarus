unit Matrix;

{$mode objfpc}{$H+}

{
TODO:
sum,
resta,
multiply,
power,
inverse,
division,
determinant,
transpose,
triangularization (gauss jordan),
multiply by scalar,
traze (traza)
}

interface
 uses
     Classes, SysUtils, Graphics, strutils,
  FileUtil, Forms, Controls, ExtCtrls,  Grids, RegExpr, utils_commands;
 type
    m_Matrix = array of array of real;

 type TMatrix = class
    A,Indetity :m_Matrix;
    MethodType: Integer;
    re_matrix: TRegExpr;
    Constructor Create(const n,m:Integer);
    function execute(C:TMatrix;B:TMatrix;d:real):TMatrix;
    Function MSuma(B:TMatrix) : TMatrix;
    Function MResta(B:TMatrix) : TMatrix;
    Function MMultiply(C:TMatrix;B:TMatrix) : TMatrix;
    Function MMultScalar(B:TMatrix;n:Real) : TMatrix;
    Function MTranspose(B:TMatrix) : TMatrix;
    Function MSubMatrix(B:TMatrix;i:Integer;j:Integer): TMatrix;
    Function MDeterminant(B:TMatrix) : Real;
    Function MAdjunt(B:TMatrix) : TMatrix;
    Function MInverse(B:TMatrix) : TMatrix;
    Function To_String(): String;
    function To_reuse_String(): String;
    procedure GridToMatrix(ma : TStringGrid);
    function read_from_string(input : String; VAR matrix_result: TMatrix; only_validation : Boolean = True):Boolean;
  end;

 const
   F_sum = 0;
   F_rest = 1;
   F_mul = 2;
   F_mule = 3;
   F_tra = 4;
   F_sub = 5;
   F_det = 6;
   F_adj = 7;
   F_inv = 8;
implementation

Constructor TMatrix.Create(const n,m:Integer);
begin
  re_matrix := TRegExpr.Create('^\[(.*?)\]$');
  SetLength(A,n,m);
end;

function TMatrix.execute(C:TMatrix;B:TMatrix;d:real): TMatrix;
begin
     case MethodType of
       F_sum: Result:= MSuma(B);
       F_rest: Result:= MResta(B);
       F_mul : Result:= MMultiply(C,B);
       F_mule : Result:= MMultScalar(B,d);
       F_tra : Result:= MTranspose(B);
       //F_sub : Result:= MSubMatrix(B,i,j);
       //F_det : Result:= MDeterminant(B);
       F_adj : Result:= MAdjunt(B);
       F_inv : Result:= MInverse(B);
     end;
end;

function TMatrix.To_String(): String;
var i,j,m,n: Integer;
begin
  m:=Length(A);n:=Length(A[0]);
  Result:= '';
  for i:=0 to m-1 do
  begin
      for j:=0 to n-1 do
      begin
          Result:= Result + FloatToStr(A[i][j]) + '  ';
      end;
      Result:= Result + #10;
  end;
end;

function TMatrix.To_reuse_String(): String;
var i,j,m,n: Integer;
begin
  m:=Length(A);n:=Length(A[0]);
  Result:= '[';
  for i:=0 to m-1 do
  begin
      for j:=0 to n-1 do
      begin
          Result:= Result + FloatToStr(A[i][j]) + ' ';
      end;
      if (i<>m-1) then
      begin
         Result:= Result + ';';
      end;
  end;
  Result:= Result + ']';
end;

Function TMatrix.MSuma(B:TMatrix) : TMatrix;
var i,j,ma,na,mb,nb:integer ;
      res:TMatrix;
begin
         ma:=Length(A);na:=Length(A[0]);
         mb:=Length(B.A);nb:=Length(B.A[0]);
         res:=TMatrix.Create(ma,na);

         for i:=0 to ma-1 do
             for j:=0 to na-1 do
                 begin
                   res.A[i,j]:=(A[i,j]+B.A[i,j]);
                 end;
         Result:=res;
end;

Function TMatrix.MResta(B:TMatrix) : TMatrix;
var i,j,ma,na,mb,nb:integer ;
    res:TMatrix;
begin
         ma:=Length(A);na:=Length(A[0]);
         mb:=Length(B.A);nb:=Length(B.A[0]);
         res:=TMatrix.Create(ma,na);

         if not ((ma=mb) and (na=nb))then exit;
         for i:=0 to ma-1 do
             for j:=0 to na-1 do
                 begin
                   res.A[i,j]:=(A[i,j]-B.A[i,j]);
                 end;
         Result:=res;
    end;

Function TMatrix.MMultiply(C:TMatrix;B:TMatrix) : TMatrix;
var i,j,h,ma,na,mb,nb:integer ;
    res:TMatrix;
begin
     ma:=Length(C.A);na:=Length(C.A[0]);
     mb:=Length(B.A);nb:=Length(B.A[0]);
     res:=TMatrix.Create(ma,nb);
     if not (na=mb)then exit;
     for i:=0 to ma-1 do
         for j:=0 to nb-1 do
             begin
                  res.A[i,j]:=0;
                  for h:=0 to na-1 do
                  begin
                       res.A[i,j]:=res.A[i,j]+(C.A[i,h]*B.A[h,j]);
                  end;
             end;
     Result:=res;
end;

Function TMatrix.MMultScalar(B:TMatrix;n:Real) : TMatrix;
var i,j,ma,na:integer ;
    res:TMatrix;
begin
     ma:=Length(B.A);na:=Length(B.A[0]);
     res:=TMatrix.Create(ma,na);
     for i:=0 to ma-1 do
         for j:=0 to na-1 do
         begin
              res.A[i,j]:=B.A[i,j]*n;
         end;
     Result:=res;
end;

Function TMatrix.MTranspose(B:TMatrix) : TMatrix;
var i,j,ma,na:integer ;
    res:TMatrix;
begin
     ma:=Length(B.A);na:=Length(B.A[0]);
     res:=TMatrix.Create(na,ma);
     for i:=0 to ma-1 do
         for j:=0 to na-1 do
             begin
                  res.A[j,i]:=B.A[i,j];
             end;
     Result:=res;
end;

Function TMatrix.MSubMatrix(B:TMatrix;i:Integer;j:Integer): TMatrix;
var x,y,w,z,ma,na:integer;
    res:TMatrix;
begin
     ma:=Length(B.A);na:=Length(B.A[0]);
     res:=TMatrix.Create(ma-1,na-1);
     w:=0;z:=0;
     for x:=0 to ma-1 do
     begin
          if(x<>i)then
          begin
               for y:=0 to na-1 do
               if((x<>i)and(y<>j))then
               begin
                    res.A[w,z]:=B.A[x,y];
                    z:=z+1
               end;
              w:=w+1;
              z:=0;
          end;
     end;
     Result:=res;
end;

Function TMatrix.MDeterminant(B:TMatrix) : Real;
var s,k,ma,na:integer ;
begin
     result:=0.0;
     ma:=Length(B.A);
     if ma>1 then na:=Length(B.A[0])
     else
         begin
              result:=B.A[0,0];
              exit;
         end;
     if not (ma=na)then exit;
     if(ma=2) then result:=B.A[0,0]*B.A[1,1]-B.A[0,1]*B.A[1,0]
     else if ma=3 then
          result:=( B.A[0,0]*B.A[1,1]*B.A[2,2]+B.A[2,0]*B.A[0,1]*B.A[1,2]+
            B.A[1,0]*B.A[2,1]*B.A[0,2]-B.A[2,0]*B.A[1,1]*B.A[0,2]-B.A[1,0]*
            B.A[0,1]*B.A[2,2]-B.A[0,0]*B.A[2,1]*B.A[1,2] )
     else
     begin
          s:=1;
          for k:=0 to na-1 do
          begin
               result:=result+s*B.A[0,k]*MDeterminant(MSubMatrix(B,0,k));
               s:=s*-1;
          end;
     end;
end;

Function TMatrix.MAdjunt(B:TMatrix) : TMatrix;
var i,j,s,s1,ma,na:integer;
res,temp:TMatrix;
begin
     ma:=Length(B.A);na:=Length(B.A[0]);
     if not (ma=na)then exit;
     res:=TMatrix.Create(ma,na);
     temp:=TMatrix.Create(ma-1,na-1);
     s1:=1;
     for i:=0 to ma-1 do
     begin
          s:=s1;
          for j:=0 to na-1 do
          begin
               temp:=MSubMatrix(B,i,j);;
               res.A[i,j]:=s*MDeterminant(temp);
               s:=s*(-1);
          end;
          s1:=s1*(-1);
     end;
     Result:=res;
end;

Function TMatrix.MInverse(B:TMatrix) : TMatrix;
var ma,na:Integer ;
    det:Real;
    adj,res:TMatrix;
begin
     ma:=Length(B.A);na:=Length(B.A[0]);
     if not (ma=na)then exit;
     adj:=TMatrix.Create(ma,na);
     res:=TMatrix.Create(ma,na);
     adj:=MAdjunt(B);
     det:=MDeterminant(B);
     if det=0 then exit;
     adj:=MTranspose(adj);
     det:=1/det;
     res:=MMultScalar(adj,det);
     Result:=res;
end;

procedure TMatrix.GridToMatrix(ma : TStringGrid);
var
  m,n,j,i:Integer;
begin
  m:=Length(A);n:=Length(A[0]);
  for j:=0 to m-1 do
    for i:=0 to n-1 do
      A[j,i]:=StrToFloat(ma.Cells[i,j]);
end;

function TMatrix.read_from_string(input : String; VAR matrix_result: TMatrix; only_validation : Boolean = True):Boolean;
var
  m,n,j,i:Integer;
  count_dimensions: Integer;
  splited, elements: TStringList;
  tmp_string, tmp_element: String;
begin
  Result := False;
  if re_matrix.Exec(input) then begin
       splited := TStringList.Create;

       tmp_string:= Copy(re_matrix.Match[0],2,Length(re_matrix.Match[0])-2);
       ExtractStrings([';'],[],PChar(tmp_string),splited);

       elements := TStringList.Create;
       ExtractStrings([' '],[],PChar(splited[0]),elements);

       matrix_result := TMatrix.Create(splited.Count, elements.Count);

       for i:=0 to splited.Count-1 do
       begin
            elements := TStringList.Create;
            ExtractStrings([' '],[],PChar(splited[i]),elements);
            if ((i>0) and (count_dimensions <> elements.Count)) then
            begin
                 Exit;
            end;
            count_dimensions:=elements.Count;
            for j:=0 to elements.Count-1 do
            begin
                 tmp_element := StringReplace(elements[j],' ','', [rfReplaceAll]);
                 if not check_is_real(tmp_element) then
                 begin
                      Exit;
                 end;
                 matrix_result.A[i,j] := StrToFloat(tmp_element);
            end;
       end;
       Result := True;
  end;
end;

end.
