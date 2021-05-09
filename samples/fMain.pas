unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  CheckTreeView;

type
  TForm6 = class(TForm)
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1KeyPress(Sender: TObject; var Key: Char);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    procedure FillLevelNodes(ParentNode: TTreeNode; Level: Integer);
    procedure FillNodes;
    procedure CheckStateChanged(Node: TTreeNode);
  public
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.CheckStateChanged(Node: TTreeNode);
const
  StateNames: array[TCheckState] of string = ('unchecked', 'checked', 'partial');
begin
  Memo1.Lines.Add(Format('%s new check state: %s', [Node.Text, StateNames[Node.CheckState]]));
end;

procedure TForm6.FillLevelNodes(ParentNode: TTreeNode; Level: Integer);
var
  I: Integer;
  Node: TTreeNode;
  NodeName: string;
begin
  for I := 1 to 2 do
  begin
    if ParentNode <> nil then
      NodeName := ParentNode.Text + '.' + IntToStr(I)
    else
      NodeName := 'Node ' + IntToStr(I);
    Node := TreeView1.Items.AddChild(ParentNode, NodeName);
    if Level < 3 then
      FillLevelNodes(Node, Level + 1);
  end;
end;

procedure TForm6.FillNodes;
begin
  TreeView1.Items.BeginUpdate;
  try
    FillLevelNodes(nil, 1);
    TreeView1.FullExpand;
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  TreeView1.EnableTristateCheckboxes;
  FillNodes;
end;

procedure TForm6.TreeView1KeyPress(Sender: TObject; var Key: Char);
begin
  TreeView1.HandleKeyPress(Key, CheckStateChanged);
end;

procedure TForm6.TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TreeView1.HandleMouseDown(Button, Shift, X, Y, CheckStateChanged);
end;

end.
