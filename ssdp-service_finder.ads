with Ada.Containers.Vectors;

package SSDP.Service_Finder is

   function To_US(Str: String) return Unbounded_String
     renames To_Unbounded_String;

   type Finder_Device_Type is new Device_Type with private;

   function Initialize_Device(Service_Type, Universal_Serial_Number: in String)
			     return Finder_Device_Type;

   procedure Set_Universal_Serial_Number(Device: in out Finder_Device_Type;
					 Universal_Serial_Number: in String);

   procedure Set_Service_Type(Device: in out Finder_Device_Type;
			      Service_Type: in String);

   function Get_Universal_Serial_Number(Device: in Finder_Device_Type)
				       return String;

   function Get_Service_Type(Device: in Finder_Device_Type) return String;

   procedure M_Search(Device: in out Finder_Device_Type;
		      Other_Header: in Message_Header_Array);

   procedure Start_Listening(Device: in out Finder_Device_Type);

   procedure Stop_Listening(Device: in out Finder_Device_Type);
private
   use Ada.Containers;

   subtype Service_Count_Type is Count_Type range 1..100;

   type Service_Device_Type is new Device_Type with record
      Location,
      Expires: Unbounded_String;
   end record;

   package Service_Vectors is new Vectors(Service_Count_Type,
					  Service_Device_Type);

   type Finder_Device_Type is new Device_Type with record
      Services: Service_Vectors.Vector := Service_Vectors.Empty_Vector;
   end record;
end SSDP.Service_Finder;
