object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'CheckTreeView Sample'
  ClientHeight = 503
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 10
  Padding.Top = 10
  Padding.Right = 10
  Padding.Bottom = 10
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 10
    Top = 361
    Width = 322
    Height = 5
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = 0
    ExplicitTop = 305
    ExplicitWidth = 342
  end
  object TreeView1: TTreeView
    Left = 10
    Top = 10
    Width = 322
    Height = 351
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Indent = 19
    TabOrder = 0
    OnKeyPress = TreeView1KeyPress
    OnMouseDown = TreeView1MouseDown
  end
  object Memo1: TMemo
    Left = 10
    Top = 366
    Width = 322
    Height = 127
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 1
  end
end
