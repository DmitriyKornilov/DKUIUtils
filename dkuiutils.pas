{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DKUIUtils;

{$warn 5023 off : no warning about unused units}
interface

uses
  DK_Progress, DK_Zoom, DK_Filter, DK_ColorLegend, DK_ControlView, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DKUIUtils', @Register);
end.
