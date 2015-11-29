with Ada.Unchecked_Conversion;

package body SSDP.Utils is
   use Gnat.Sockets;

   function To_Char is new Ada.Unchecked_Conversion(Stream_Element, Character);
   function To_String(Msg: in Stream_Element_Array) return String is
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

   procedure Activate_Multicast_Connection is
      GMC: Multicast_Connection renames Global_Multicast_Connection;
   begin
      if not GMC.Is_Down then
	 return;
      end if;

      -- is DOWN and NOT listening
      GMC.Address.Addr := Inet_Addr(Multicast_Address);
      GMC.Address.Port := Multicast_Port;

      Create_Socket(GMC.Socket, Family_Inet, Socket_Datagram);
      Set_Socket_Option(GMC.Socket, Socket_Level, (Reuse_Address, True));
      Set_Socket_Option(GMC.Socket, Ip_Protocol_For_Ip_Level,
			(Add_Membership, GMC.Address.Addr, Any_Inet_Addr));

      Bind_Socket(GMC.Socket, GMC.Address);

      GMC.Channel := Stream(GMC.Socket, GMC.Address);
      GMC.Is_Down := False;

      -- UP but still NOT listening
   end Activate_Multicast_Connection;

   procedure Send_Message(Device: in out Device_Type; Message: in String) is
   begin
      if Global_Multicast_Connection.Is_Down then
	 Activate_Multicast_Connection;
      end if;

      String'Write(Global_Multicast_Connection.Channel, Message);
   end Send_Message;

end SSDP.Utils;