object EditorForm: TEditorForm
  Left = 298
  Top = 143
  Width = 696
  Height = 393
  Caption = 'EditorForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Scintilla1: TScintilla
    Left = 0
    Top = 0
    Width = 688
    Height = 347
    Align = alClient
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 8
    object miFont: TMenuItem
      Caption = 'Font'
      OnClick = miFontClick
    end
    object Unicode1: TMenuItem
      Caption = 'Unicode'
      object Read1: TMenuItem
        Caption = 'Read'
        OnClick = Read1Click
      end
    end
  end
end
