package body SSDP.Utils is
   use Gnat.Sockets;

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

   function Activate_Connection return Broadcast_Connection is
      BC: Broadcast_Connection; -- is DOWN and NOT listening
   begin
      BC.Address.Addr := Inet_Addr(Multicast_Address);
      BC.Address.Port := Multicast_Port;
      Create_Socket(BC.Socket, Family_Inet, Socket_Datagram);
      Set_Socket_Option(BC.Socket, Ip_Protocol_For_Ip_Level,
			(Add_Membership, BC.Address.Addr, Any_Inet_Addr));
      BC.Channel := Stream(BC.Socket, BC.Address);
      BC.Is_Down := False;

      return BC; -- UP but still NOT listening
   end Activate_Connection;

   procedure Send_Message(Device: in out Device_Type; Message: in String) is
   begin
      if Device.Connection.Is_Down then
	 Device.Connection := Activate_Connection;
      end if;

      String'Output(Device.Connection.Channel, Message);
   end Send_Message;

end SSDP.Utils;
