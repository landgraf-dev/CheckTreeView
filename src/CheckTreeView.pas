unit CheckTreeView;

interface

uses
  Winapi.Windows, Winapi.CommCtrl, System.Classes, Vcl.Controls, Vcl.ComCtrls;

type
  TCheckState = (csUnchecked, csChecked, csPartial);

  TCheckStateChangedProc = reference to procedure(Node: TTreeNode);

  TTreeNodeHelper = class helper for TTreeNode
  strict private
    function GetCheckState: TCheckState;
    procedure SetCheckState(const Value: TCheckState);
  public
    procedure UpdateParentCheckState;
    procedure SetChildrenCheckState(State: TCheckState);
    procedure DoCheckStateChanged(OnCheckStateChanged: TCheckStateChangedProc);
    property CheckState: TCheckState read GetCheckState write SetCheckState;
  end;

  TTreeViewHelper = class helper for TTreeView
  public
    procedure EnableTristateCheckboxes;
    procedure HandleKeyPress(var Key: Char; OnCheckStateChanged: TCheckStateChangedProc = nil);
    procedure HandleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      OnCheckStateChanged: TCheckStateChangedProc = nil);
  end;

implementation

{ TTreeNodeHelper }

procedure TTreeNodeHelper.DoCheckStateChanged(OnCheckStateChanged: TCheckStateChangedProc);
var
  State: TCheckState;
  Node: TTreeNode;
begin
  Node := Self;
  State := Node.CheckState;
  if State = csPartial then
  begin
    State := csUnchecked;
    Node.CheckState := State;
  end;
  Node.SetChildrenCheckState(State);
  Node.UpdateParentCheckState;
  if Assigned(OnCheckStateChanged) then
    OnCheckStateChanged(Node);
end;

function TTreeNodeHelper.GetCheckState: TCheckState;
var
  State: UINT;
begin
  State := TreeView_GetItemState(TreeView.Handle, Self.ItemId, TVIS_STATEIMAGEMASK);
  Result := TCheckState((State shr 12) - 1);
end;

procedure TTreeNodeHelper.SetCheckState(const Value: TCheckState);
begin
  TreeView_SetItemState(TreeView.Handle, Self.ItemId, (Ord(Value) + 1) shl 12, TVIS_STATEIMAGEMASK);
end;

procedure TTreeNodeHelper.SetChildrenCheckState(State: TCheckState);
var
  Child: TTreeNode;
begin
  Child := Self.getFirstChild;
  while Child <> nil do
  begin
    Child.SetChildrenCheckState(State);
    Child.CheckState := State;
    Child := Child.getNextSibling;
  end;
end;

procedure TTreeNodeHelper.UpdateParentCheckState;
var
  checkCount: Cardinal;
  uncheckCount: Cardinal;
  mixedCount: Cardinal;
  checkState: TCheckState;
  childNode: TTreeNode;
  node: TTreeNode;
begin
  Node := Self.Parent;
  if Node = nil then Exit;

  checkCount := 0;
  uncheckCount := 0;
  mixedCount := 0;
  childNode := node.GetFirstChild;
  while Assigned(childNode) do
  begin
    case childNode.CheckState of
      csChecked:
        Inc(checkCount);
      csPartial:
        Inc(mixedCount);
      csUnchecked:
        Inc(uncheckCount);
    end;
    childNode := childNode.GetNextSibling;
  end;

  if mixedCount > 0 then
    checkState := csPartial
  else
    if checkCount > 0 then
    begin
      if uncheckCount > 0 then
        checkState := csPartial
      else
        checkState := csChecked;
    end else
      checkState := csUnchecked;

  node.CheckState := checkState;
  node.UpdateParentCheckState;
end;

{ TTreeViewHelper }

procedure TTreeViewHelper.EnableTristateCheckboxes;
begin
  TreeView_SetExtendedStyle(Self.Handle, TVS_EX_PARTIALCHECKBOXES, TVS_EX_PARTIALCHECKBOXES);
end;

procedure TTreeViewHelper.HandleKeyPress(var Key: Char; OnCheckStateChanged: TCheckStateChangedProc = nil);
begin
  if (Key = #32) and Assigned(Selected) then
    Selected.DoCheckStateChanged(OnCheckStateChanged);
end;

procedure TTreeViewHelper.HandleMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer; OnCheckStateChanged: TCheckStateChangedProc = nil);
var
  Node: TTreeNode;
begin
  if (htOnStateIcon in Self.GetHitTestInfoAt(X, Y)) then
  begin
    Node := Self.GetNodeAt(X, Y);
    if Node = nil then
      Node := Self.Selected;

    Node.DoCheckStateChanged(OnCheckStateChanged);
  end;
end;

end.
