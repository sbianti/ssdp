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

with Ada.Streams;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with Ada.Containers.Vectors;

with Gnat.Sockets;

with SSDP.Utils;

package body SSDP.Service_Finder is
   use SSDP.Utils, Gnat.Sockets, Ada.Containers;

   type Service_Device_Type is new Device_Type with record
      Location,
      Expires: Unbounded_String;
   end record;

   -- Only one service_finder should usually exist, however, we choose to allow
   --  the existence of several ones.
   -- This vector contains the list of the discovered devices and permit to
   --  manage the received events for every declared service_finder.
   subtype Service_Count_Type is Count_Type range 1..100;
   package Service_Vectors is new Vectors(Service_Count_Type,
					  Service_Device_Type);
   Services: Service_Vectors.Vector;

   function Initialize_Device(Service_Type, Universal_Serial_Number: in String)
			     return SSDP_Client is
      Device: SSDP_Client;
   begin
      if Service_Type = "" or Universal_Serial_Number = "" then
	 raise Header_Malformed
	   with "service_type AND universal_serial_number should be not null";
      end if;

      Set_Networking;

      Device.Service_Type := To_US(Service_Type);
      Device.Universal_Serial_Number := To_US(Universal_Serial_Number);

      return Device;
   end Initialize_Device;

   procedure Set_Universal_Serial_Number(Device: in out SSDP_Client;
					 Universal_Serial_Number: in String) is
   begin
      Device.Universal_Serial_Number := To_US(Universal_Serial_Number);
   end Set_Universal_Serial_Number;

   procedure Set_Service_Type(Device: in out SSDP_Client;
			      Service_Type: in String) is
   begin
      Device.Service_Type := To_US(Service_Type);
   end Set_Service_Type;

   function Get_Universal_Serial_Number(Device: in SSDP_Client)
				       return String is
   begin
      return To_String(Device.Universal_Serial_Number);
   end Get_Universal_Serial_Number;

   function Get_Service_Type(Device: in SSDP_Client) return String is
   begin
      return To_String(Device.Service_Type);
   end Get_Service_Type;

   procedure M_Search(Device: in SSDP_Client;
		      Other_Header: in Message_Header_Array) is
      Start_Line: constant String := M_Search_Star_Line & EOL;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «ST» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «S» (Universal Service Type) is missing";
      end if;

      Send_Message(Create_Message(Start_Line & "ST: " &
				    To_String(Device.Service_Type) & EOL &
				    "S: " &
				    To_String(Device.Universal_Serial_Number) &
				    EOL & "Man: ""ssdp:discover""" & EOL,
				  Other_Header));
   end M_Search;

   procedure Finder_Job is
      use Ada.Streams, Ada.Exceptions;

      Msg: Stream_Element_Array(1..500);
      Last: Stream_Element_Offset;
      Addr: Sock_Addr_Type;

      procedure Parse_Message(Message: in String) is
	 use Ada.Characters.Handling, Ada.Strings.Fixed;

	 procedure Get_Notify_Info(Lines: in Line_Array) is
	    use Ada.Strings;

	    Device: Device_Type;
	    NTS_Line: Natural := 0;
	    Posn_USN, Posn_NT, Posn_NTS: Natural;
	    First, Last: Natural;
	 begin
	    for I in Lines'Range loop
	       Posn_USN := Index(To_Upper(Lines(I).all), "USN:");
	       if Posn_USN > 1 then raise SSDP_Message_Malformed
		 with "USN doesn't begin at character 0";
	       elsif Posn_USN = 1 then
		  First := Lines(I)'First + 4;
		  Last := Lines(I)'Last;
		  Device.Universal_Serial_Number :=
		    To_Unbounded_String(Trim(Lines(I)(First..Last), Both));
		  goto Continue;
	       end if;

	       Posn_NT := Index(To_Upper(Lines(I).all), "NT:");
	       if Posn_NT > 1 then raise SSDP_Message_Malformed
		 with "NT doesn't begin at character 0";
	       elsif Posn_NT = 1 then
		  First := Lines(I)'First + 3;
		  Last := Lines(I)'Last;
		  Device.Service_Type :=
		    To_Unbounded_String(Trim(Lines(I)(First..Last), Both));
		  goto Continue;
	       end if;

	       Posn_NTS := Index(To_Upper(Lines(I).all), "NTS:");
	       if Posn_NTS > 1 then raise SSDP_Message_Malformed
		 with "NTS doesn't begin at character 0";
	       elsif Posn_NTS = 1 then
		  NTS_Line := I;
		  goto Continue;
	       end if;

	       Pl_Debug("Extra info: [" & Lines(I).all & "]");
	   <<Continue>>
	    end loop;

	    if NTS_Line = 0 then raise SSDP_Message_Malformed
	      with "No NTS field found";
	    elsif Device.Universal_Serial_Number = "" then
	       raise SSDP_Message_Malformed with "No USN field found";
	    elsif Device.Service_Type = "" then
	       raise SSDP_Message_Malformed with "No ST field found";
	    else
	       declare
		  Last: Natural := Lines(NTS_Line).all'Last;
		  First: Natural := Lines(NTS_Line).all'First + 5;
		  NTS: String := Trim(Lines(NTS_Line)(First..Last), Both);

		  procedure Update(USN, NT: in Unbounded_String) is
		  begin
		     for I in 1..Services.Length loop
			if Services.Element(I).Service_Type = NT and
			  Services.Element(I).Universal_Serial_Number = USN then
			   Pl_Debug("Update device: " & To_String(USN) & ' ' &
				      To_String(NT));
			   return;
			end if;
		     end loop;

		     Pl_Debug("Adding device: " & To_String(USN) & ' ' &
				To_String(NT));
		     Services.Append((NT, USN, To_US(""), To_US("")));
		  end Update;

		  procedure Bye_Bye(USN, NT: in Unbounded_String) is
		  begin
		     for I in 1..Services.Length loop
			if Services.Element(I).Service_Type = NT and
			  Services.Element(I).Universal_Serial_Number = USN then
			   Pl_Debug("Remove device: " & To_String(USN) & ' ' &
				      To_String(NT));
			   Services.Delete(I);
			   return;
			end if;
		     end loop;

		     Pl_Debug("Untracked device: " & To_String(USN) & ' ' &
				To_String(NT));
		  end Bye_Bye;
	       begin
		  if NTS = "ssdp:byebye" then
		     Pl_Debug("It's a byebye :¯(");
		     Bye_Bye(Device.Universal_Serial_Number,
			     Device.Service_Type);
		  elsif NTS = "ssdp:alive" then
		     Pl_Debug("It's an alive :)");
		     Update(Device.Universal_Serial_Number,
			    Device.Service_Type);
		  else raise SSDP_Message_Malformed
		    with "Unknown NTS field: " & NTS;
		  end if;
	       end;
	    end if;
	 end Get_Notify_Info;

	 Lines: Line_Array := Parse_Lines(Message);
	 Posn_M_SEARCH, Posn_Notify, Posn_Reply_M_SEARCH: Natural;
      begin
	 Posn_M_SEARCH := Index(To_Upper(Lines(1).all),
				M_Search_Star_Line);
	 if Posn_M_SEARCH > 1 then raise SSDP_Message_Malformed
	   with "M-SEARCH line doesn't begin at character 0";
	 elsif Posn_M_SEARCH = 1 then
	    Pl_Debug("M-search received");
	    return;
	 end if;

	 Posn_Notify := Index(To_Upper(Lines(1).all), Notify_Line);
	 if Posn_Notify > 1 then raise SSDP_Message_Malformed
	   with "Notify line doesn't begin at character 0";
	 elsif Posn_Notify = 1 then
	    Pl_Debug("Notify received");
	    Get_Notify_Info(Lines(2..Lines'Last));
	    return;
	 end if;

	 Posn_Reply_M_SEARCH := Index(To_Upper(Lines(1).all),
				      Status_Line);
	 if Posn_Reply_M_SEARCH > 1 then raise SSDP_Message_Malformed
	   with "Reply to M-SEARCH line doesn't begin at character 0";
	 elsif Posn_Reply_M_SEARCH = 1 then
	    Pl_Debug("Reply to M-SEARCH received");
	    return;
	 end if;

	 raise Not_An_SSDP_Message
	   with "Neither M-Search nor Notify nor a reply to M-Search";
      exception
	 when Ex: SSDP_Message_Malformed =>
	    Pl_Debug(Exception_Name(Ex) & ": " & Exception_Message(Ex));
      end Parse_Message;
   begin
      Set_Socket_Option(Global_Multicast_Connection.Socket,
			Ip_Protocol_For_Ip_Level, (Multicast_Loop, True));

      loop
	 begin
	    Receive_Socket(Global_Multicast_Connection.Socket, Msg, Last, Addr);
	    Pl_Debug("____________________________________________________");
	    Pl_Debug("From " & Image(Addr));
	    Parse_Message(To_String(Msg(1..Last)));
	    Pl_Debug("¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯");
	 exception
	    when E: Not_An_SSDP_Message | SSDP_Message_Malformed =>
	       Pl_Debug(Exception_Message(E));
	 end;
      end loop;
   end Finder_Job;

   procedure Start_Listening is
   begin
      SSDP.Utils.Start_Listening(Finder_Job'Access);
   end Start_Listening;

   procedure Stop_Listening is
   begin
      SSDP.Utils.Stop_Listening;
   end Stop_Listening;

end SSDP.Service_Finder;
