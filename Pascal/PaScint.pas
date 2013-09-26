{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PaScint;

interface

uses
  PaScintilla, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PaScintilla', @PaScintilla.Register);
end;

initialization
  RegisterPackage('PaScint', @Register);
end.
