unit utrees;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, uarrays, uallocators;

type

  { mnnode }
  mncompare_func<T> = function(a, b: T): boolean;
  mnnode = record
  type pmnnode = ^mnnode;
  var
    parent: integer;
    data_index: integer;
    childs: mnarray<integer>;
    procedure init;
  end;

const
  null_node: mnnode = (parent: -1; data_index: -1;
    childs: (Data: nil; capacity: -1; Count: -1));
  { mntree }
type

  { mntree }

  mntree<T> = record
  private
    function get_root: mnnode.pmnnode;
  public
    data_array: mnarray<T>;
    nodes_array: mnarray<mnnode>;
    func_equal: mncompare_func<T>;
    func_greater: mncompare_func<T>;
    property root: mnnode.pmnnode read get_root;
    function add(item: T; parent_ind: integer): integer;
    function get_node_unsafe(index: integer): mnnode.pmnnode;
    function get_node__from_val_unsafe(Value: T): mnnode.pmnnode;
    function get_node_index_from_value(Value: T): integer;
    procedure init(func_equal: mncompare_func<T> = nil;
      func_greater: mncompare_func<T> = nil);
    procedure romove_node(index: integer);
    procedure move_node(node_index, new_parent_index: integer);
    procedure init_node_from_id_arrays(var arr_id_nodes: mnarray<integer>;
      var array_id_parent: mnarray<integer>);
  end;

type
  mntree_int = mntree<integer>;
  mntree_string = mntree<string>;

procedure mntree_int_init(var tree: mntree_int);
procedure mntree_string_init(var tree: mntree_string);

implementation



{ mntree }

function mntree<T>.get_root: mnnode.pmnnode;
begin
  Result := @(nodes_array.Data[0]);
end;

function mntree<T>.add(item: T; parent_ind: integer): integer;
var
  parent: mnnode.pmnnode;
  node: mnnode;
begin
  data_array.add(item);
  node.Init;
  node.data_index := data_array.Count - 1;
  nodes_array.add(node);
  Result := nodes_array.Count - 1;
  parent := @(nodes_array.Data[parent_ind]);
  parent.childs.add(Result);

end;

function mntree<T>.get_node_unsafe(index: integer): mnnode.pmnnode;
begin
  Result := @(nodes_array.Data[index]);
end;

function mntree<T>.get_node__from_val_unsafe(Value: T): mnnode.pmnnode;
var
  i: integer;
begin
  if not Assigned(func_equal) then
    raise Exception.Create('mnnode func_equal is not assigned');
  for i := 0 to self.nodes_array.Count - 1 do
  begin
    if self.func_equal(self.data_array.Data[self.nodes_array.Data[i].data_index],
      Value) then
    begin
      Result := @(self.nodes_array.Data[i]);
      exit;
    end;
  end;
  Result := @(null_node);
end;

function mntree<T>.get_node_index_from_value(Value: T): integer;
var
  i: integer;
begin
  if not Assigned(func_equal) then
    raise Exception.Create('mnnode func_equal is not assigned');
  for i := 0 to self.nodes_array.Count - 1 do
  begin
    if self.func_equal(self.data_array.Data[self.nodes_array.Data[i].data_index],
      Value) then
    begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;

end;

procedure mntree<T>.init(func_equal: mncompare_func<T>; func_greater: mncompare_func<T>);
var
  node: mnnode;
begin
  self.data_array.init;
  nodes_array.init;
  node.init;
  nodes_array.add(node);
  self.func_equal := func_equal;
  self.func_greater := func_greater;
end;

procedure mntree<T>.romove_node(index: integer);
var
  parent_ind, i: integer;
  parent_node, node: mnnode.pmnnode;
begin
  parent_ind := self.nodes_array.Data[index].parent;
  parent_node := @(nodes_array.Data[parent_ind]);
  node := @(nodes_array.Data[index]);
  for i := 0 to nodes_array.Data[parent_ind].childs.Count - 1 do
  begin
    if nodes_array.Data[parent_ind].childs.Data[i] = index then
    begin
      nodes_array.Data[parent_ind].childs.delete_item_at_ind(i);
      break;
    end;
  end;
  parent_node.childs.add_array(node.childs);
  nodes_array.delete_item_at_ind(index);
end;

procedure mntree<T>.move_node(node_index, new_parent_index: integer);
var
  parent_ind, i: integer;
  node, parent_node, new_parent_node: mnnode.pmnnode;
begin
  parent_ind := self.nodes_array.Data[node_index].parent;
  parent_node := @(nodes_array.Data[parent_ind]);
  node := @(nodes_array.Data[node_index]);
  new_parent_node := @(nodes_array.Data[new_parent_index]);
  if new_parent_node = parent_node then exit;
  for i := 0 to parent_node.childs.Count - 1 do
  begin
    if parent_node.childs.Data[i] = node_index then
    begin
      parent_node.childs.delete_item_at_ind(i);
      break;
    end;
  end;
  node.parent := new_parent_index;
  new_parent_node.childs.add(node_index);
end;

procedure mntree<T>.init_node_from_id_arrays(var arr_id_nodes: mnarray<integer>;
  var array_id_parent: mnarray<integer>);
var
  ind_child,ind_parent, i: integer;
begin
  mntree_int_init(mntree_int(self));
  for i := 0 to arr_id_nodes.Count - 1 do
  begin
    ind_parent:=mntree_int(self).get_node_index_from_value(array_id_parent.data[i]);
    if ind_parent =-1 then
    begin
       ind_parent := mntree_int(self).add(array_id_parent.Data[i], 0);
    end;
    ind_child:= mntree_int(self).get_node_index_from_value(arr_id_nodes.data[i]);
    if ind_child = -1 then
    begin
      ind_child := mntree_int(self).add(arr_id_nodes.Data[i], ind_parent);
    end
    else
    begin
     self.move_node(ind_child,ind_parent);
    end;
  end;
end;

function int_equal(a, b: integer): boolean;
begin
  Result := (a = b);
end;

function int_greater(a, b: integer): boolean;
begin
  Result := (a > b);
end;

procedure mntree_int_init(var tree: mntree_int);
begin
  tree.init(int_equal, int_greater);
end;

function str_equal(a, b: string): boolean;
begin
  Result := (a = b);
end;

function str_greater(a, b: string): boolean;
begin
  Result := (a > b);
end;

procedure mntree_string_init(var tree: mntree_string);
begin
  tree.init(str_equal, str_greater);
end;

{ mnnode }

procedure mnnode.init;
begin
  self.childs.init;
  self.data_index := -1;
  self.parent := -1;
end;

end.
