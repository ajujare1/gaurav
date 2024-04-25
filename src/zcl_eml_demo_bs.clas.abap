CLASS zcl_eml_demo_bs DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_eml_demo_bs IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.


    out->write( 'Before Update' ).

    READ ENTITIES OF zi_ga_trav_bs " BO Name
      ENTITY Travel                " Entity
      ALL FIELDS
       WITH VALUE #( ( TravelUuid = '75844BA0799075DB1800133A36269004' ) )
       RESULT DATA(travel)
       FAILED DATA(Failed)
       REPORTED DATA(Reported).

   MODIFY ENTITIES OF zi_ga_trav_bs
    ENTITY Travel
       CREATE
        SET FIELDS WITH VALUE #( ( %cid = 'ID_1'
                                   TravelId = '205'
                                   AgencyId = '070041'
                                   CustomerId = '000594'
                                   BeginDate = cl_abap_context_info=>get_system_date(  )
                                   EndDate = cl_abap_context_info=>get_system_date(  ) + 5
                                   Description = 'My SAP Create Entry' ) ).

    COMMIT ENTITIES
        RESPONSE OF zi_ga_trav_bs FAILED DATA(Failed_com).


    READ ENTITIES OF zi_ga_trav_bs
       ENTITY Travel BY \_Booking
       ALL FIELDS WITH
       VALUE #( ( TravelUuid = '75844BA0799075DB1800133A36269004' ) )
       RESULT DATA(Booking).
    out->write( Travel ).
*    out->write( Booking ).
  ENDMETHOD.
ENDCLASS.
