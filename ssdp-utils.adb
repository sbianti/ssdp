--------------------------------------------------------------------------------
--  This file is part of SSDP package   				      --
--  									      --
--  Copyright © 2015 Sébastien Bianti					      --
--  									      --
--  This program is free software; you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License version 3 as	      --
--  published by the Free Software Foundation.				      --
--  									      --
--  This program is distributed in the hope that it will be useful,	      --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of	      --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	      --
--  GNU General Public License for more details.			      --
--  									      --
--  You should have received a copy of the GNU General Public License	      --
--  along with this program; if not, write to the Free Software	              --
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA --
--------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Exceptions;

package body SSDP.Utils is
   use Gnat.Sockets;

   task body Listener is
      use Ada.Exceptions;

      Job: Job_Procedure_Access;
   begin
      accept Start(Job: in Job_Procedure_Access) do
	 Listener.Job := Job;
      end Start;

      Job.all;
   exception
      when E: others => Pl_Error("Task listener: " & Exception_Message(E));
   end Listener;

   procedure Pl_Error(Str: in String) is
      use Ada.Text_IO;
   begin
      Put_Line(Standard_Error, "Error: " & Str);
   end Pl_Error;

   procedure Pl_Warning(Str: in String) is
      use Ada.Text_IO;
   begin
      Put_Line(Standard_Error, "Warning: " & Str);
   end Pl_Warning;

   procedure Pl_Debug(Str: in String) is
      use Ada.Text_IO;
   begin
      pragma Debug(Put_Line(Str));
   end Pl_Debug;

   function To_String(Msg: in Stream_Element_Array) return String is
      function To_Char is new Ada.Unchecked_Conversion(Stream_Element,
						       Character);
      Str: String(1..Msg'Length);
   begin
      for I in Msg'Range loop
	 Str(Natural(I)) := To_Char(Msg(I));
      end loop;

      return Str;
   end To_String;

   function Create_Message(Static_Part: in String;
			   Headers: in Message_Header_Array) return String is
      Size: Natural := Static_Part'Length + Headers'Length * EOL'Length +
	EOL'Length; -- the final EOL
   begin
      for N in Headers'Range loop
	 Size := Size + Length(Headers(N));
      end loop;

      declare
	 Message: String(1..Size);
	 Index: Positive := Static_Part'Length;
	 Last: Natural;
      begin
	 Message(1..Index) := Static_Part;
	 Index := Index + 1;

	 for N in Headers'Range loop
	    Last := Index + Length(Headers(N)) + EOL'Length - 1;

	    Message(Index..Last) := To_String(Headers(N)) & EOL;
	    Index := Last + 1;
	 end loop;

	 Message(Index..Message'Last) := EOL;

	 return Message;
      end;
   end Create_Message;

   procedure Set_Networking is
      GNS: Network_Settings renames Global_Network_Settings;
      Multicast_Addr: Sock_Addr_Type;
   begin
      if not GNS.Is_Down then
	 return;
      end if;

      -- is DOWN and NOT listening
      Create_Socket(GNS.Socket(Multicast), Family_Inet, Socket_Datagram);

      Set_Socket_Option(GNS.Socket(Multicast), Socket_Level,
			(Reuse_Address, True));

      Multicast_Addr.Addr := Inet_Addr(Multicast_Address);
      Multicast_Addr.Port := Multicast_Port;

      Set_Socket_Option(GNS.Socket(Multicast), Ip_Protocol_For_Ip_Level,
			(Add_Membership, Multicast_Addr.Addr, Any_Inet_Addr));

      Bind_Socket(GNS.Socket(Multicast), Multicast_Addr);

      Create_Socket(GNS.Socket(Unicast), Family_Inet, Socket_Datagram);
      GNS.Local_Address.Addr := Any_Inet_Addr;
      GNS.Local_Address.Port := Any_Port;

      Bind_Socket(GNS.Socket(Unicast), GNS.Local_Address);
      GNS.Channel := Stream(GNS.Socket(Unicast), Multicast_Addr);

      GNS.Is_Down := False;

      -- UP but still NOT listening
   end Set_Networking;

   procedure Send_Message(Message: in String) is
   begin
      if Global_Network_Settings.Is_Down then
	 Set_Networking;
      end if;

      String'Write(Global_Network_Settings.Channel, Message);
   end Send_Message;

   procedure Send_Message(Message: in String; Address: in Sock_Addr_Type) is
      Channel: Stream_Access;
   begin
      if Global_Network_Settings.Is_Down then
	 Set_Networking;
      end if;

      Channel := Stream(Global_Network_Settings.Socket(Unicast), Address);

      String'Write(Channel, Message);
   end Send_Message;

   function Parse_Lines(Message: in String) return Line_Array is
      use Ada.Strings.Fixed;

      Number_Of_Lines: Natural;
      -- A body could be in this message, so we need to get the
      -- first position of a double EOL:
      Posn_Of_Double_EOL: Natural;
   begin
      if Message'Length < SSDP_Message_Min_Size then
	 raise Not_An_SSDP_Message
	   with "message a way to short to be an ssdp message (" &
	   Natural'Image(Message'Length) & " character(s))";
      end if;

      Posn_Of_Double_EOL := Index(Message, EOL & EOL);
      Number_Of_Lines := -- +1 is for: -1 + EOL'Length
	Count(Message(Message'First..Posn_Of_Double_EOL + 1), EOL);

      declare
	 Lines: Line_Array(1..Number_Of_Lines);
	 From, Posn: Natural;
	 LF: constant String := EOL(2..2);
      begin
	 From := Message'First;

	 for I in Lines'Range
	 loop
	    Posn := Index(Message, EOL, From);
	    Lines(I) := new String'(Message(From..Posn - 1));
	    -- if the line contains a line feed, the line is broken:
	    if Index(Lines(I).all, LF) /= 0 then
	       raise SSDP_Message_Malformed
		 with "a line itself contains an end of line:" &
		 "[" & Lines(I).all & "]";
	    end if;
	    From := Posn + 2;
	 end loop;

	 return Lines;
      end;
   end Parse_Lines;

   procedure Start_Listening(Job: in Job_Procedure_Access) is
   begin
      if not Global_Network_Settings.Is_Listening then
	 Listener.Start(Job);
	 Global_Network_Settings.Is_Listening := True;
      end if;
   end Start_Listening;

   procedure Stop_Listening is
      Addr: Inet_Addr_Type := Inet_Addr(Multicast_Address);
   begin
      abort Listener;

      if Global_Network_Settings.Is_Listening then
	 Set_Socket_Option(Global_Network_Settings.Socket(Multicast),
			   Ip_Protocol_For_Ip_Level,
			   (Drop_Membership, Addr, Any_Inet_Addr));

	 Close_Socket(Global_Network_Settings.Socket(Multicast));
	 Close_Socket(Global_Network_Settings.Socket(Unicast));
	 Free(Global_Network_Settings.Channel);

	 Global_Network_Settings.Is_Listening := False;
      end if;
   end Stop_Listening;

end SSDP.Utils;
