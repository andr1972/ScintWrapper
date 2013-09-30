object Form1: TForm1
  Left = 192
  Top = 107
  Width = 483
  Height = 417
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    475
    390)
  PixelsPerInch = 96
  TextHeight = 13
  object Scintilla1: TScintilla
    Left = 24
    Top = 8
    Width = 441
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Button1: TButton
    Left = 368
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Open'
    TabOrder = 1
    OnClick = Button1Click
  end
end
