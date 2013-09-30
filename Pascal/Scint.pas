{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Scint;

interface

uses
  Scintilla, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Scintilla', @Scintilla.Register);
end;

initialization
  RegisterPackage('Scint', @Register);
end.
