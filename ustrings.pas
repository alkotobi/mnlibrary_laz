unit ustrings;
{$mode ObjFPC}{$H+}
interface

uses
  Classes,
  SysUtils,
  Character,
  StrUtils;

const
  tachkil_array = [1611, 1612, 1613, 1614, 1615, 1616, 1617, 1618];
  tachkil_start = 1611;
  tachkil_end = 1618;
  horof_start = 1569;
  horof_end = 1610;
  matta_int = 1600;




function delete_repeated_spaces(const s: string): string;

function string_is_numiric(str: string): boolean;

function string_is_date(str: string): boolean;

function remove_tachkil(nass: WideString): string;

function string_to_words(nass: string): TStringArray;

function remove_extra_spaces_for_sql_text(sql_text: string): string;

function remove_space(str: string): string;
function remove_matta(str: WideString): string;
function get_clean_text(str: WideString): string;

implementation

function string_is_date(str: string): boolean;
var
  v: TDateTime;
begin
  Result := TryStrToDate(str, v);
end;

function string_is_numiric(str: string): boolean;
var
  val: extended;
begin
  Result := TryStrToFloat(str, val);
end;

function remove_tachkil(nass: WideString): string;
var
  chr: widechar;
begin
  Result := '';
  for chr in nass do
  begin
    if (integer(chr) > tachkil_end) or (integer(chr) < tachkil_start) then
      Result := Result + string(chr);
  end;
end;

function string_to_words(nass: string): TStringArray;
begin
  Result := SplitString(delete_repeated_spaces(nass), ' ');
end;

function remove_extra_spaces_for_sql_text(sql_text: string): string;
var
  i, Count: integer;
begin
  i := 1;
  Count := 1;
  Result := '';
  SetLength(Result, length(sql_text));

  while i <= length(sql_text) do
  begin
    if (Count = 1) and (IsWhiteSpace(sql_text[i])) then
    begin
      i := i + 1;
      continue;
    end;
    if (IsWhiteSpace(sql_text[i])) then
    begin
      if (i = length(sql_text)) or (IsWhiteSpace(sql_text[i + 1])) or
        (sql_text[i + 1] = ',') or (sql_text[i - 1] = ',') then
      begin
        i := i + 1;
        continue;
      end;

    end;
    //while (i < length(sql_text) - 1) and (sql_text[i + 1] = ' ') do
    //begin
    //  i := i + 1;

    //  if sql_text[i + 1] = ',' then
    //    i := i + 1;
    //  if (sql_text[i - 1] = ',') then
    //    i := i + 1;

    //end;

    Result[Count] := sql_text[i];
    i := i + 1;
    Count := Count + 1;

  end;
  //if Result[Count - 1] = ' ' then
  //  Count := Count - 2;
  SetLength(Result, Count - 1);
end;

function remove_space(str: string): string;
var
  i, Count: integer;
begin
  i := 1;
  Count := 1;
  Result := '';
  SetLength(Result, length(str));
  while i <= Length(str) do
  begin
    if IsWhiteSpace(str[i]) then
    begin
      i := i + 1;
      continue;
    end;
    Result[Count] := str[i];
    i := i + 1;
    Count := Count + 1;
  end;
  SetLength(Result, Count - 1);
end;

function remove_matta(str: WideString): string;
var
  chr: widechar;
begin
  Result := '';
  for chr in str do
  begin
    if integer(chr) <> matta_int then
      Result := Result + string(chr);
  end;
end;

function get_clean_text(str: WideString): string;
var
  chr: widechar;
  space:string;
begin
  Result := '';
  space:='';
  for chr in str do
  begin
    if integer(chr) = matta_int then Continue;
    if IsWhiteSpace(chr) then
    begin
       space :=' ';
         Continue;
    end;
    if (Character.IsLetterOrDigit(unicodechar(chr))) then
    begin
      Result := result+space+string(chr);
      space:='';
      Continue;
    end;
    //if (integer(chr) >= horof_start) and (integer(chr) <= horof_end) then
    //  Result := Result + string(chr);
  end;
end;

function delete_repeated_spaces(const s: string): string;
var
  i, j, State: integer;
begin
  result:='';
  SetLength(Result, Length(s));
  j := 0;
  State := 0;

  for i := 1 to Length(s) do
  begin

    if s[i] = ' ' then
      Inc(State)
    else
      State := 0;

    if State < 2 then
      Result[i - j] := s[i]
    else
      Inc(j);
  end;

  if j > 0 then
    SetLength(Result, Length(s) - j);
end;




end.
