  unit diferential_solver;

  {$mode objfpc}

  interface

  uses
    Classes, SysUtils, ParseMath, math, fgl;

  type
     DiferentialSolver = class
         x_results: TList;
         y_results: TList;
         Parse: TParseMath;
         function simple_euler(xn, end_xn, h, f_xn: Real; fxy: String): Real;
         function optimized_euler(xn, end_xn, h, f_xn: Real; fxy: String): Real;
         function runge_kutta(xn, end_xn, h, f_xn: Real; fxy: String): Real;
         function dormand_prince(xn, end_xn, h, f_xn: Real; fxy: String): Real;
         function fxy_eval(x,y: Real; fxy: String): Real;

         public
             constructor create;
     end;


  implementation
  constructor DiferentialSolver.create;
  begin
    Parse:= TParseMath.create();
    Parse.AddVariable('x', 0);
    Parse.AddVariable('y', 0);
    x_results:= TList.Create;
    y_results:= TList.Create;

  end;

  function DiferentialSolver.fxy_eval(x,y: Real; fxy: String): Real;
  begin
    Parse.Expression:= fxy;
    Parse.NewValue('x', x);
    Parse.NewValue('y', y);
    Result:= Parse.Evaluate();

  end;

  function DiferentialSolver.simple_euler(xn, end_xn, h, f_xn: Real; fxy: String): Real;
  var tmp_fxn: Real;
    show_str: String;
  begin
    Parse.Expression:= fxy;

    while xn <= end_xn-h do
    begin
      show_str:= FloatToStr(xn)+ '   '+FloatToStr(f_xn) + #13#10;
      Write(show_str);
      x_results.Add(Pointer(xn));
      y_results.Add(Pointer(f_xn));

      Parse.NewValue('x', xn);
      Parse.NewValue('y', f_xn);
      tmp_fxn:= Parse.Evaluate();
      f_xn:= f_xn + (h*tmp_fxn);
      xn:=xn+h;
    end;
    Result:= f_xn;
    show_str:= FloatToStr(xn)+ '   '+FloatToStr(f_xn) + #13#10;
    Write(show_str);
    x_results.Add(Pointer(xn));
    y_results.Add(Pointer(f_xn));
  end;

  function DiferentialSolver.runge_kutta(xn, end_xn, h, f_xn: Real; fxy: String): Real;
  var tmp_fxn: Real;
    show_str: String;
    k1, k2, k3, k4: Real;
    m: Real;
  begin
    Parse.Expression:= fxy;

    while xn <= end_xn-h do
    begin
      show_str:= FloatToStr(xn)+ '   '+FloatToStr(f_xn) + #13#10;
      Write(show_str);
      x_results.Add(Pointer(xn));
      y_results.Add(Pointer(f_xn));


      //Calculating k1
      Parse.NewValue('x', xn);
      Parse.NewValue('y', f_xn);
      tmp_fxn:= Parse.Evaluate();
      k1:= tmp_fxn;

      //Calculating k2
      Parse.NewValue('x', xn + h/2);
      Parse.NewValue('y', f_xn + (k1*h/2) );
      tmp_fxn:= Parse.Evaluate();
      k2:= tmp_fxn;

      //Calculating k3
      Parse.NewValue('x', xn + h/2);
      Parse.NewValue('y', f_xn + (k2*h/2) );
      tmp_fxn:= Parse.Evaluate();
      k3:= tmp_fxn;

      //Calculating k4
      Parse.NewValue('x', xn + h);
      Parse.NewValue('y', f_xn + (k3*h) );
      tmp_fxn:= Parse.Evaluate();
      k4:= tmp_fxn;

      m:= (1/6)*(k1+2*k2+2*k3+k4);
      f_xn:= f_xn + (h*m);
      xn:=xn+h;
    end;
    Result:= f_xn;
    show_str:= FloatToStr(xn)+ '   '+FloatToStr(f_xn) + #13#10;
    Write(show_str);
    x_results.Add(Pointer(xn));
    y_results.Add(Pointer(f_xn));
  end;

  function DiferentialSolver.dormand_prince(xn, end_xn, h, f_xn: Real; fxy: String): Real;
  var tmp_fxn: Real;
    show_str: String;
    a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65: Real;
    a71, a72, a73, a74, a75, a76: Real;
    c2, c3, c4, c5, c6, c7, delta: Real;
    b1, b2, b3, b4, b5, b6, b7: Real;
    b1p, b2p, b3p, b4p, b5p, b6p, b7p: Real;
    K1, K2, K3, K4, K5, K6, K7: Real;
    x, y, y0, x0, x1: Real;
    h_max, h_min, tol, error: Real;
    max_iter, i: Integer;
  begin
    Parse.Expression:= fxy;
    x0:= xn;
    y0:= f_xn;
    x1:= end_xn;
    h_max:=1;
    tol:= 1.0e-5;
    h_min:=0.01;
    max_iter:= 1000;

    x    := x0;
    y    := y0;
    h    := h_max;

    a21  :=  (1.0/5.0);
    a31  := (3.0/40.0);
    a32  := (9.0/40.0);
    a41  := (44.0/45.0);
    a42  := (-56.0/15.0);
    a43   := (32.0/9.0);
    a51  :=  (19372.0/6561.0);
    a52  := (-25360.0/2187.0);
    a53   := (64448.0/6561.0);
    a54   := (-212.0/729.0);
    a61   := (9017.0/3168.0);
    a62   := (-355.0/33.0);
    a63   := (46732.0/5247.0);
    a64  := (49.0/176.0);
    a65  := (-5103.0/18656.0);
    a71  := (35.0/384.0);
    a72  := (0.0);
    a73  := (500.0/1113.0);
    a74  := (125.0/192.0);
    a75  := (-2187.0/6784.0);
    a76  := (11.0/84.0);

    c2   := (1.0 / 5.0);
    c3   := (3.0 / 10.0);
    c4   := (4.0 / 5.0);
    c5   := (8.0 / 9.0);
    c6   := (1.0);
    c7   := (1.0);

    b1   := (35.0/384.0);
    b2   := (0.0);
    b3   := (500.0/1113.0);
    b4   := (125.0/192.0);
    b5   := (-2187.0/6784.0);
    b6   := (11.0/84.0);
    b7   := (0.0);

    b1p  := (5179.0/57600.0);
    b2p  := (0.0);
    b3p  := (7571.0/16695.0);
    b4p  := (393.0/640.0);
    b5p  := (-92097.0/339200.0);
    b6p  := (187.0/2100.0);
    b7p  := (1.0/40.0);

    for i:=0 to max_iter-1 do
    begin
      show_str:= FloatToStr(x)+ '   '+FloatToStr(y) + #13#10;
      Write(show_str);
      x_results.Add(Pointer(x));
      y_results.Add(Pointer(y));
      K1 := fxy_eval(x,y, fxy);
      K2 := fxy_eval(x+ c2*h, y+h*(a21*K1), fxy);
      K3 := fxy_eval(x+ c3*h, y+h*(a31*K1+a32*K2), fxy);
      K4 := fxy_eval(x+ c4*h, y+h*(a41*K1+a42*K2+a43*K3), fxy);
      K5 := fxy_eval(x+ c5*h, y+h*(a51*K1+a52*K2+a53*K3+a54*K4), fxy);
      K6 := fxy_eval(x+    h, y+h*(a61*K1+a62*K2+a63*K3+a64*K4+a65*K5), fxy);
      K7 := fxy_eval(x+    h, y+h*(a71*K1+a72*K2+a73*K3+a74*K4+a75*K5+a76*K6), fxy);

      error := abs((b1-b1p)*K1+(b3-b3p)*K3+(b4-b4p)*K4+(b5-b5p)*K5+(b6-b6p)*K6+(b7-b7p)*K7);

      delta := 0.84 * power(tol / error, (1.0/5.0));

      if error < tol then begin
        x := x+h;
        y := y + h*(b1*K1+b3*K3+b4*K4+b5*K5+b6*K6);
      end;

      if delta <= 0.1 then begin
        h := h*0.1
      end
      else if delta >= 4.0 then begin
        h := h*4.0;
      end
      else begin
        h := h*delta;
      end;


      if h>h_max then begin
        h := h_max;
      end;
      //
      if x >= x1 then begin
        Break;
      end
      else if x+h>x1 then begin
        h:= x1-x;
      end
      else if (h < h_min) then begin
        Break;
      end;

    end;
    Result:= y;
    show_str:= FloatToStr(x)+ '   '+FloatToStr(y) + #13#10;
    Write(show_str);
    x_results.Add(Pointer(x));
    y_results.Add(Pointer(y));
  end;



  function DiferentialSolver.optimized_euler(xn, end_xn, h, f_xn: Real; fxy: String): Real;
  var tmp_fxn, tmp_fxn_2: Real;
    show_str: String;
    fxn_ast: Real;
  begin
    Parse.Expression:= fxy;
    while xn <= end_xn-h do
    begin
      show_str:= FloatToStr(xn)+ '   '+FloatToStr(f_xn) + #13#10;
      Write(show_str);
      x_results.Add(Pointer(xn));
      y_results.Add(Pointer(f_xn));

      Parse.NewValue('x', xn);
      Parse.NewValue('y', f_xn);
      tmp_fxn:= Parse.Evaluate();
      xn:=xn+h;

      fxn_ast:=f_xn + h*tmp_fxn;
      Parse.NewValue('x', xn);
      Parse.NewValue('y', fxn_ast);
      tmp_fxn_2:= Parse.Evaluate();

      f_xn:= f_xn + ( (h/2)*(tmp_fxn+tmp_fxn_2) );
    end;
    Result:= f_xn;
    show_str:= FloatToStr(xn)+ '   '+FloatToStr(f_xn) + #13#10;
    Write(show_str);
    x_results.Add(Pointer(xn));
    y_results.Add(Pointer(f_xn));
  end;

  end.

