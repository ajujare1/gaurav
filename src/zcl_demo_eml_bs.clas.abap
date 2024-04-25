CLASS zcl_demo_eml_bs DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_eml_bs IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    READ ENTITIES OF zi_ga_trav
        ENTITY Travel
        ALL FIELDS
         WITH VALUE #( ( TravelUuid = '78844BA0799075DB1800133A36269004' ) )
*        WITH VALUE #( ( TravelUuid = '111111111111111111111111111111' ) )
         RESULT DATA(travels)
         FAILED DATA(failed)
         REPORTED DATA(reported).

    READ ENTITIES OF zi_ga_trav
        ENTITY Travel BY \_Booking
        ALL FIELDS WITH VALUE #( ( TravelUuid = '78844BA0799075DB1800133A36269004' ) )
         RESULT DATA(bookings).
    out->write( travels ).
*    out->write( bookings ).
  ENDMETHOD.
ENDCLASS.
