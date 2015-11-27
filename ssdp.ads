with Ada.Strings.Unbounded;

package SSDP is
   use Ada.Strings.Unbounded;

   function To_US(Str: String) return Unbounded_String
     renames To_Unbounded_String;

   type US_Array is array (Positive range <>) of Unbounded_String;
   subtype Location_Array is US_Array;
   subtype Message_Header_Array is US_Array;

    ----------------------------------------------------------------------------
    --  SSDP messages are http 1.1 messages where:			      --
    --   ⋅service type URI represents the type of service (ex: refrigerator)  --
    --   ⋅USN is a URI which uniquely identifies a particular instance of a   --
    --  service, ex: upnp:uuid:bla:… for a device that has a uuid             --
    ----------------------------------------------------------------------------

   --  type SSDP_Record_Type(Location_Number: Natural) is record
   --     Service_Type_Uri: Unbounded_String;
   --     Unique_Service_Name_Uri: Unbounded_String;
   --     Expiration_Announcement: Calendar.Time;
   --     Location: Location_Array(1..Location_Number);
   --     ST_Header: Unbounded_String;
   --  end record;

   function M_Search(Service_Type: in String;
		     Universal_Serial_Number: in String;
		     Other_Header: in Message_Header_Array) return String;

   function M_Search_Response(Service_Type: in String;
			      Universal_Serial_Number: in String;
			      USN_Requester: in String;
			      Other_Headers: in Message_Header_Array)
			     return String;

   function Notify_Alive(Service_Type: in String;
			 Universal_Serial_Number: in String;
			 Location, AL: in String; -- only one is needed
			 Cache_Control, Expires: in String;
			 Other_Headers: in Message_Header_Array) return String;

   function Notify_Byebye(Service_Type: in String;
			  Universal_Serial_Number: in String) return String;

   procedure Send_Message(Message: in String);

   Header_Malformed: exception;

end SSDP;
