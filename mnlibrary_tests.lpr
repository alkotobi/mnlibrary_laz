program mnlibrary_tests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, customdrawn, liteprovider10, myprovider10, dac10, unidac10, umain,
  udb_basic, udb_types, ujson, uarrays, udataset, ustrings, upaths, ugui,
  udb_tools_design
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

