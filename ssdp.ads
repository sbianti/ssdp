with Ada.Strings.Unbounded;
private with Gnat.Sockets;

package SSDP is
   use Ada.Strings.Unbounded;

   type US_Array is array (Positive range <>) of Unbounded_String;
   subtype Location_Array is US_Array;
   subtype Message_Header_Array is US_Array;

   type Device_Type is tagged private;

    ----------------------------------------------------------------------------
    --  SSDP messages are http 1.1 messages where:			      --
    --   ⋅service type URI represents the type of service (ex: refrigerator)  --
    --   ⋅USN is a URI which uniquely identifies a particular instance of a   --
    --  service, ex: upnp:uuid:bla:… for a device that has a UUID             --
    ----------------------------------------------------------------------------

   --  type SSDP_Record_Type(Location_Number: Natural) is record
   --     Service_Type_Uri: Unbounded_String;
   --     Unique_Service_Name_Uri: Unbounded_String;
   --     Expiration_Announcement: Calendar.Time;
   --     Location: Location_Array(1..Location_Number);
   --     ST_Header: Unbounded_String;
   --  end record;

   Header_Malformed: exception;

private
   use Gnat.Sockets;

   type Broadcast_Connection is record
      Is_Listening: Boolean := False;
      Is_Down: Boolean := True;
      Socket: Socket_Type;
      Address: Sock_Addr_Type;
      Channel: Stream_Access;
   end record;

   type Device_Type is tagged record
      Service_Type,
      Universal_Serial_Number: Unbounded_String;
      Connection: Broadcast_Connection;
   end record;

end SSDP;
