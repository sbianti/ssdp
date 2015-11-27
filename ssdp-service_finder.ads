package SSDP.Service_Finder is

   function To_US(Str: String) return Unbounded_String
     renames To_Unbounded_String;

   type Device_Type is private;

   function Initialize_Device(Service_Type, Universal_Serial_Number: in String)
			     return Device_Type;

   procedure Set_Universal_Serial_Number(Device: in out Device_Type;
					 Universal_Serial_Number: in String);

   procedure Set_Service_Type(Device: in out Device_Type;
			      Service_Type: in String);

   function Get_Universal_Serial_Number(Device: in Device_Type) return String;

   function Get_Service_Type(Device: in Device_Type) return String;

   procedure M_Search(Device: in Device_Type;
		      Other_Header: in Message_Header_Array);

private
   type Device_Type is record
      Service_Type,
      Universal_Serial_Number: Unbounded_String;
   end record;
end SSDP.Service_Finder;
