with Ada.Streams;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Exceptions;

with Gnat.Sockets;

with SSDP.Utils;

package body SSDP.Service_Finder is
   use SSDP.Utils, Gnat.Sockets;

   function Initialize_Device(Service_Type, Universal_Serial_Number: in String)
			     return Finder_Device_Type is
      Device: Finder_Device_Type;
   begin
      if Service_Type = "" or Universal_Serial_Number = "" then
	 raise Header_Malformed
	   with "service_type AND universal_serial_number should be not null";
      end if;

      Activate_Multicast_Connection;

      Device.Service_Type := To_US(Service_Type);
      Device.Universal_Serial_Number := To_US(Universal_Serial_Number);

      return Device;
   end Initialize_Device;

   procedure Set_Universal_Serial_Number(Device: in out Finder_Device_Type;
					 Universal_Serial_Number: in String) is
   begin
      Device.Universal_Serial_Number := To_US(Universal_Serial_Number);
   end Set_Universal_Serial_Number;

   procedure Set_Service_Type(Device: in out Finder_Device_Type;
			      Service_Type: in String) is
   begin
      Device.Service_Type := To_US(Service_Type);
   end Set_Service_Type;

   function Get_Universal_Serial_Number(Device: in Finder_Device_Type)
				       return String is
   begin
      return To_String(Device.Universal_Serial_Number);
   end Get_Universal_Serial_Number;

   function Get_Service_Type(Device: in Finder_Device_Type) return String is
   begin
      return To_String(Device.Service_Type);
   end Get_Service_Type;

   procedure M_Search(Device: in out Finder_Device_Type;
		      Other_Header: in Message_Header_Array) is
      Start_Line: constant String := M_Search_Star_Line & EOL;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «ST» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «S» (Universal Service Type) is missing";
      end if;

      Send_Message(Device_Type(Device),
		   Create_Message(Start_Line & "ST: " &
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

	 type Line_Array is array (Natural range <>) of access String;

	 function Parse_Lines(Message: in String) return Line_Array is
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

	 procedure Get_Notify_Info(Lines: in Line_Array) is
	    use Ada.Strings;

	    Device: Device_Type;
	    NTS_Index: Natural := 0;
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
		    To_Unbounded_String(Trim(Lines(I)(First..Last),
					     Both));
		  goto Continue;
	       end if;

	       Posn_NT := Index(To_Upper(Lines(I).all), "NT:");
	       if Posn_NT > 1 then raise SSDP_Message_Malformed
		 with "NT doesn't begin at character 0";
	       elsif Posn_NT = 1 then
		  First := Lines(I)'First + 3;
		  Last := Lines(I)'Last;
		  Device.Service_Type :=
		    To_Unbounded_String(Trim(Lines(I)(First..Last),
					     both));
		  goto Continue;
	       end if;

	       Posn_NTS := Index(To_Upper(Lines(I).all), "NTS:");
	       if Posn_NTS > 1 then raise SSDP_Message_Malformed
		 with "NTS doesn't begin at character 0";
	       elsif Posn_NTS = 1 then
		  NTS_Index := I;
		  goto Continue;
	       end if;

	       Pl_Debug("Extra info: [" & Lines(I).all & "]");
	       << Continue >>
	    end loop;

	       if NTS_Index = 0 then raise SSDP_Message_Malformed
		 with "No NTS field found";
	       elsif Device.Universal_Serial_Number = "" then
		  raise SSDP_Message_Malformed with "No USN field found";
	       elsif Device.Service_Type = "" then
		  raise SSDP_Message_Malformed with "No ST field found";
	       else
		  declare
		     Last: Natural := Lines(NTS_Index).all'Last;
		     First: Natural := Lines(NTS_Index).all'First + 5;
		     NTS: String := Trim(Lines(NTS_Index)(First..Last),
					 Side => Both);
		  begin
		     if NTS = "ssdp:byebye" then
			Pl_Debug("It's a byebye :¯(");
		     elsif NTS = "ssdp:alive" then
			Pl_Debug("It's an alive :)");
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
			Ip_Protocol_For_Ip_Level, (Multicast_Loop, False));

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
      SSDP.Utils.Start_Listening(Finder_Job'access);
   end Start_Listening;

   procedure Stop_Listening is
   begin
      SSDP.Utils.Stop_Listening;
   end Stop_Listening;

end SSDP.Service_Finder;
