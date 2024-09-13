{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit modern_gui;

{$warn 5023 off : no warning about unused units}
interface

uses
  PanelRounded, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PanelRounded', @PanelRounded.Register);
end;

initialization
  RegisterPackage('modern_gui', @Register);
end.
