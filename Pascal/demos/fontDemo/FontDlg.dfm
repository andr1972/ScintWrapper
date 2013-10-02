object FontDialog: TFontDialog
  Left = 483
  Top = 180
  Width = 292
  Height = 370
  Caption = 'FontDialog'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object seSize: TSpinEdit
    Left = 208
    Top = 8
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 10
    OnChange = seSizeChange
  end
  object btnDefaultFont: TButton
    Left = 200
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Default font'
    TabOrder = 1
  end
  object btnDefaultSize: TButton
    Left = 200
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Default size'
    TabOrder = 2
  end
  object ListView: TListView
    Left = 8
    Top = 8
    Width = 177
    Height = 321
    Columns = <
      item
        Width = 150
      end>
    TabOrder = 3
    ViewStyle = vsReport
    OnChange = ListViewChange
  end
end
