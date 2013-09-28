object Form1: TForm1
  Left = 192
  Top = 107
  Width = 474
  Height = 433
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PaScintilla1: TPaScintilla
    Left = 24
    Top = 16
    Width = 433
    Height = 345
  end
  object Button1: TButton
    Left = 300
    Top = 370
    Width = 75
    Height = 25
    Caption = 'Messages'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 380
    Top = 370
    Width = 75
    Height = 25
    Caption = 'Direct'
    TabOrder = 2
    OnClick = Button2Click
  end
end
