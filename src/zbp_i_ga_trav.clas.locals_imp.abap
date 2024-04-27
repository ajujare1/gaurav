CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF travel_status,
        open     TYPE c LENGTH 1  VALUE 'O', " Open
        accepted TYPE c LENGTH 1  VALUE 'A', " Accepted
        canceled TYPE c LENGTH 1  VALUE 'X', " Cancelled
      END OF travel_status.
*    METHODS get_instance_features FOR INSTANCE FEATURES
*      IMPORTING keys REQUEST requested_features FOR Travel RESULT result.

    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel.

    METHODS recalcTotalPrice FOR MODIFY
      IMPORTING keys FOR ACTION Travel~recalcTotalPrice.

    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~calculateTotalPrice.

    METHODS setInitialStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setInitialStatus.

    METHODS calculateTravelID FOR DETERMINE ON SAVE
      IMPORTING keys FOR Travel~calculateTravelID.

    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateCustomer.

    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

*  METHOD get_instance_features.
*  ENDMETHOD.

  METHOD acceptTravel.
      " Set the new overall status
    MODIFY ENTITIES OF zi_ga_trav IN LOCAL MODE
      ENTITY Travel
         UPDATE
           FIELDS ( Status )
           WITH VALUE #( FOR key IN keys
                           ( %tky         = key-%tky
                             Status = travel_status-accepted ) )
      FAILED failed
      REPORTED reported.
      " Fill the response table
    READ ENTITIES OF zi_ga_trav IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

*    result = VALUE #( FOR travel IN travels
*                        ( %tky   = travel-%tky
*                          %param = travel ) ).
  ENDMETHOD.

  METHOD recalcTotalPrice.
  ENDMETHOD.

  METHOD rejectTravel.
        " Set the new overall status
    MODIFY ENTITIES OF zi_ga_trav IN LOCAL MODE
      ENTITY Travel
         UPDATE
           FIELDS ( Status )
           WITH VALUE #( FOR key IN keys
                           ( %tky         = key-%tky
                             Status = travel_status-canceled ) )
      FAILED failed
      REPORTED reported.
      " Fill the response table
    READ ENTITIES OF zi_ga_trav IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

*    result = VALUE #( FOR travel IN travels
*                        ( %tky   = travel-%tky
*                          %param = travel ) ).
  ENDMETHOD.

  METHOD calculateTotalPrice.
  ENDMETHOD.

  METHOD setInitialStatus.
      " Read relevant travel instance data
    READ ENTITIES OF zi_ga_trav IN LOCAL MODE
      ENTITY Travel
        FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

     DELETE travels WHERE Status IS NOT INITIAL.
     CHECK travels IS NOT INITIAL.

   MODIFY ENTITIES OF zi_ga_trav IN LOCAL MODE
    ENTITY Travel
      UPDATE
        FIELDS ( Status )
        WITH VALUE #( FOR travel IN travels
                      ( %tky         = travel-%tky
                        Status = travel_status-open ) )

    REPORTED DATA(update_reported).

*    reported = CORRESPONDING #( DEEP update_reported ).
  ENDMETHOD.

  METHOD calculateTravelID.
  READ ENTITIES OF zi_ga_trav IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelID ) WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    " remove lines where TravelID is already filled.
    DELETE travels WHERE TravelID IS NOT INITIAL.

    " anything left ?
    CHECK travels IS NOT INITIAL.

    " Select max travel ID
    SELECT SINGLE
        FROM  zga_trav_bs
        FIELDS MAX( travel_id ) AS travelID
        INTO @DATA(max_travelid).

    " Set the travel ID
    MODIFY ENTITIES OF zi_ga_trav IN LOCAL MODE
    ENTITY Travel
      UPDATE
        FROM VALUE #( FOR travel IN travels INDEX INTO i (
          %tky              = travel-%tky
          TravelID          = max_travelid + i
          %control-TravelID = if_abap_behv=>mk-on ) )
    REPORTED DATA(update_reported).

    reported = CORRESPONDING #( DEEP update_reported ).
  ENDMETHOD.

  METHOD validateAgency.
  ENDMETHOD.

  METHOD validateCustomer.
  ENDMETHOD.

  METHOD validateDates.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS calculateBookingID FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booking~calculateBookingID.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booking~calculateTotalPrice.

ENDCLASS.

CLASS lhc_booking IMPLEMENTATION.

  METHOD calculateBookingID.
  ENDMETHOD.

  METHOD calculateTotalPrice.
  ENDMETHOD.

ENDCLASS.

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
