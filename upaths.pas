unit upaths;

{$mode delphi}{$H+}

interface

uses
  {$IFDEF FPC}
   LazFileUtils,
  {$ELSE}
    ioutils,
  {$ENDIF}
  Classes, SysUtils;

function get_documents_dir():string;

implementation

function get_documents_dir(): string;
begin
  {$IFDEF FPC}
    Result:= AppendPathDelim(GetUserDir + 'Documents');
  {$ELSE}
    result := TPath.GetDocumentsPath;
  {$ENDIF}
end;

end.

