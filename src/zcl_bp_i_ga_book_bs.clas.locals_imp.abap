CLASS lhc_Booking DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS setTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booking~setTotalPrice.

ENDCLASS.

CLASS lhc_Booking IMPLEMENTATION.

  METHOD setTotalPrice.

   READ ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
    ENTITY Booking BY \_Travel
    FIELDS ( TravelUuid )
    WITH CORRESPONDING #( keys )
    RESULT DATA(travels).

   MODIFY ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
    ENTITY Travel
    EXECUTE recalcTotalPrice
    FROM CORRESPONDING #( travels )
    REPORTED DATA(set_reported).

    reported = CORRESPONDING #( DEEP set_reported ).
  ENDMETHOD.

ENDCLASS.
