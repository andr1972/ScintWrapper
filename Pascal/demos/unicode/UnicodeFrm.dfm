object Form1: TForm1
  Left = 192
  Top = 107
  Width = 463
  Height = 352
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    455
    325)
  PixelsPerInch = 96
  TextHeight = 13
  object Scintilla1: TScintilla
    Left = 8
    Top = 8
    Width = 436
    Height = 281
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Button1: TButton
    Left = 368
    Top = 296
    Width = 75
    Height = 25
    Caption = 'SaveAs..'
    TabOrder = 1
    OnClick = Button1Click
  end
  object SaveDialog1: TSaveDialog
    Left = 24
    Top = 16
  end
end
