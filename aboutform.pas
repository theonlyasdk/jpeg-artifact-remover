{
	MIT License

	Copyright (c) 2025 TheOnlyASDK

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.

}

unit AboutForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  JPP.LinkLabel;

type

  { TFAbout }

  TFAbout = class(TForm)
    BClose: TButton;
    JppLinkLabel1: TJppLinkLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    MLicense: TMemo;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FAbout: TFAbout;

implementation

{$R *.lfm}

{ TFAbout }

procedure TFAbout.FormShow(Sender: TObject);
begin
     Constraints.MinWidth:=Width;
     Constraints.MinHeight:=Height;
end;

end.

