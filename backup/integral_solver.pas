unit integral_solver;

{$mode objfpc}

interface

uses
  Classes, SysUtils, ParseMath, math;

type
   IntegralSolver = class
       Parse: TParseMath;
       local_h: Real;
       is_area: Boolean;
       function trapezoidal_method(n: Integer; a,b: Real; fx: String): Real;
       function simpson_method(n: Integer; a,b: Real; fx: String): Real;
       function simpson_method_3_8(n: Integer; a,b: Real; fx: String): Real;
       public
           constructor create;
   end;

implementation
constructor IntegralSolver.create;
begin
  Parse:= TParseMath.create();
  Parse.AddVariable( 'x', 0);
  is_area:= False;
end;

function IntegralSolver.trapezoidal_method(n: Integer; a,b: Real; fx: String): Real;
var h,xi, fa, fb, sum_fxi: Real;
  i: Integer;
begin
  i:=1;
  Parse.Expression:= fx;
  h:= (b-a)/n;
  local_h:=h;
  sum_fxi:=0;

  Parse.NewValue('x', a);
  fa:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());
  //fa:= abs(Parse.Evaluate());
  Parse.NewValue('x', b);
  //fb:= abs(Parse.Evaluate());
  fb:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());
  xi:=a;
  repeat
    xi:= xi + h;
    Parse.NewValue('x', xi);
    //sum_fxi:=sum_fxi + abs(Parse.Evaluate());
    sum_fxi:=sum_fxi + ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());
    i:= i+1;
  until (i >= n);
  Result:= (fa+fb)/2;
  Result:= Result + sum_fxi;
  Result:= Result * h;

end;
function IntegralSolver.simpson_method(n: Integer; a,b: Real; fx: String): Real;
var h,xi, fa, fb, sum_odd_fxi, sum_even_fxi: Real;
  tmp_eval: Real;
  i: Integer;
begin
  i:=1;
  Parse.Expression:= fx;
  n:= n*2;
  h:= (b-a)/(n);
  local_h:=h;

  sum_odd_fxi:=0;
  sum_even_fxi:=0;

  Parse.NewValue('x', a);
  fa:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());

  Parse.NewValue('x', b);
  fb:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());


  xi:= a + ((2*i)*h);
  while xi < b do
  begin
    Parse.NewValue('x', xi);
    tmp_eval:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());
    sum_even_fxi:=sum_even_fxi + tmp_eval;
    i:= i+1;
    xi:= a + ((2*i)*h);
  end;



  i:=0;
  xi:= a + ((2*i+1)*h);
  while xi < b do
  begin
    Parse.NewValue('x', xi);
    tmp_eval:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());
    sum_odd_fxi:=sum_odd_fxi + tmp_eval;
    i:= i+1;
    xi:= a + ((2*i+1)*h);
  end;

  Result:= 2*sum_even_fxi + 4*sum_odd_fxi;
  Result:= Result + fa + fb;
  Result:= Result * (h/3);

end;

function IntegralSolver.simpson_method_3_8(n: Integer; a,b: Real; fx: String): Real;
var h,xi, fa, fb, sum_one_fxi, sum_two_fxi, sum_three_fxi: Real;
  tmp_eval: Real;
  i: Integer;
begin
  i:=0;
  Parse.Expression:= fx;
  n:= n*2;
  h:= (b-a)/(n);

  sum_one_fxi:=0;
  sum_two_fxi:=0;
  sum_three_fxi:=0;

  Parse.NewValue('x', a);
  fa:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());

  Parse.NewValue('x', b);
  fb:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());

  xi:= a + ((3*i+1)*h);
  while xi < b do
  begin
    Parse.NewValue('x', xi);
    tmp_eval:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());
    sum_one_fxi:=sum_one_fxi + tmp_eval;
    i:= i+1;
    xi:= a + ((3*i+1)*h);
  end;

  i:=0;
  xi:= a + ((3*i+2)*h);
  while xi < b do
  begin
    Parse.NewValue('x', xi);
    tmp_eval:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());
    sum_two_fxi:=sum_two_fxi + tmp_eval;
    i:= i+1;
    xi:= a + ((3*i+2)*h);
  end;

  i:=1;
  xi:= a + ((3*i)*h);
  while xi < b do
  begin
    Parse.NewValue('x', xi);
    tmp_eval:= ifthen(is_area, abs(Parse.Evaluate()), Parse.Evaluate());
    sum_three_fxi:=sum_three_fxi + tmp_eval;
    i:= i+1;
    xi:= a + ((3*i)*h);
  end;

  Result:= 3*sum_one_fxi + 3*sum_two_fxi + 2*sum_three_fxi;
  Result:= Result + fa + fb;
  Result:= Result * (h*3/8);

end;

end.

