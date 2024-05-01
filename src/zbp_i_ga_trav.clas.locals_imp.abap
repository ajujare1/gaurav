CLASS lsc_zi_ga_trav DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zi_ga_trav IMPLEMENTATION.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF travel_status,
        open     TYPE c LENGTH 1  VALUE 'O', " Open
        accepted TYPE c LENGTH 1  VALUE 'A', " Accepted
        canceled TYPE c LENGTH 1  VALUE 'X', " Cancelled
      END OF travel_status.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Travel RESULT result.

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

    METHODS calculateTravelID FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~calculateTravelID.

    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.

    METHODS validateMandatory FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateMandatory.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.
    METHODS precheck_update FOR PRECHECK
      IMPORTING entities FOR UPDATE travel.
    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR travel RESULT result.
    METHODS is_update_granted IMPORTING has_before_image      TYPE abap_bool
                                        overall_status        TYPE /dmo/overall_status
                              RETURNING VALUE(update_granted) TYPE abap_bool.

    METHODS is_delete_granted IMPORTING has_before_image      TYPE abap_bool
                                        overall_status        TYPE /dmo/overall_status
                              RETURNING VALUE(delete_granted) TYPE abap_bool.

    METHODS is_create_granted RETURNING VALUE(create_granted) TYPE abap_bool.


ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

  METHOD get_instance_features.
    " Read the travel status of the existing travels
    READ ENTITIES OF zi_ga_trav IN LOCAL MODE
      ENTITY Travel
        FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(travels)
      FAILED failed.

    result =
      VALUE #(
        FOR travel IN travels
          LET is_accepted =   COND #( WHEN travel-Status = travel_status-accepted
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled  )
              is_rejected =   COND #( WHEN travel-Status = travel_status-canceled
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled )
          IN
            ( %tky                 = travel-%tky
              %action-acceptTravel = is_accepted
              %action-rejectTravel = is_rejected
              %assoc-_Booking = COND #( WHEN travel-Status = travel_status-accepted
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled )
             ) ).
  ENDMETHOD.

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

    TYPES: BEGIN OF ty_amount_per_currencycode,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_amount_per_currencycode.

    DATA: amount_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.

    " Read all relevant travel instances.
    READ ENTITIES OF zi_ga_trav IN LOCAL MODE
          ENTITY Travel
             FIELDS ( BookingFee CurrencyCode )
             WITH CORRESPONDING #( keys )
          RESULT DATA(travels).

    DELETE travels WHERE CurrencyCode IS INITIAL.

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      " Set the start for the calculation by adding the booking fee.
      amount_per_currencycode = VALUE #( ( amount        = <travel>-BookingFee
                                           currency_code = <travel>-CurrencyCode ) ).
      " Read all associated bookings and add them to the total price.
      READ ENTITIES OF ZI_ga_trav IN LOCAL MODE
         ENTITY Travel BY \_Booking
            FIELDS ( FlightPrice CurrencyCode )
          WITH VALUE #( ( %tky = <travel>-%tky ) )
          RESULT DATA(bookings).
      LOOP AT bookings INTO DATA(booking) WHERE CurrencyCode IS NOT INITIAL.
        COLLECT VALUE ty_amount_per_currencycode( amount        = booking-FlightPrice
                                                  currency_code = booking-CurrencyCode ) INTO amount_per_currencycode.
      ENDLOOP.

      CLEAR <travel>-TotalPrice.
      LOOP AT amount_per_currencycode INTO DATA(single_amount_per_currencycode).
        " If needed do a Currency Conversion
        IF single_amount_per_currencycode-currency_code = <travel>-CurrencyCode.
          <travel>-TotalPrice += single_amount_per_currencycode-amount.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                   =  single_amount_per_currencycode-amount
               iv_currency_code_source     =  single_amount_per_currencycode-currency_code
               iv_currency_code_target     =  <travel>-CurrencyCode
               iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                   = DATA(total_booking_price_per_curr)
            ).
          <travel>-TotalPrice += total_booking_price_per_curr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " write back the modified total_price of travels
    MODIFY ENTITIES OF ZI_ga_trav IN LOCAL MODE
      ENTITY travel
        UPDATE FIELDS ( TotalPrice )
        WITH CORRESPONDING #( travels ).
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

    MODIFY ENTITIES OF zi_ga_trav IN LOCAL MODE
      ENTITY travel
        EXECUTE recalcTotalPrice
        FROM CORRESPONDING #( keys )
      REPORTED DATA(execute_reported).

    reported = CORRESPONDING #( DEEP execute_reported ).

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

  METHOD validateDates.
  ENDMETHOD.

  METHOD validateMandatory.

    DATA lt_agency TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.
    DATA lt_customer TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.
    READ
     ENTITIES OF zi_ga_trav IN LOCAL MODE
     ENTITY Travel
        FIELDS ( AgencyID CustomerId  ) WITH CORRESPONDING #( keys )
        RESULT DATA(travel).

    lt_agency = CORRESPONDING #( travel DISCARDING DUPLICATES MAPPING agency_id = AgencyId ) .
    DELETE lt_agency WHERE agency_id IS INITIAL.

    lt_customer = CORRESPONDING #( travel DISCARDING DUPLICATES MAPPING customer_id = CustomerId ) .
    DELETE lt_customer WHERE customer_id IS INITIAL.

    LOOP AT travel INTO DATA(ls_travel).
      IF lt_agency IS INITIAL.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky = ls_travel-%tky %msg = new_message_with_text(  severity = if_abap_behv_message=>severity-error
                                                                             text = 'Agency ID can not be empty' ) )


        TO reported-travel.
      ENDIF.
      IF lt_customer IS INITIAL.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky = ls_travel-%tky %msg = new_message_with_text(  severity = if_abap_behv_message=>severity-error
                                                                             text = 'Customer ID can not be empty' ) )


        TO reported-travel.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_authorizations.

    DATA: has_before_image    TYPE abap_bool,
          is_update_requested TYPE abap_bool,
          is_delete_requested TYPE abap_bool,
          update_granted      TYPE abap_bool,
          delete_granted      TYPE abap_bool.

    READ ENTITIES OF zi_ga_trav IN LOCAL MODE
       ENTITY Travel
       FIELDS ( Status )
       WITH CORRESPONDING #( keys )
       RESULT DATA(travels).

    is_update_requested = COND #( WHEN requested_authorizations-%update              = if_abap_behv=>mk-on OR
                                   requested_authorizations-%action-acceptTravel = if_abap_behv=>mk-on OR
                                   requested_authorizations-%action-rejectTravel = if_abap_behv=>mk-on OR
                                   requested_authorizations-%action-Edit         = if_abap_behv=>mk-on OR
                                   requested_authorizations-%assoc-_Booking      = if_abap_behv=>mk-on
                              THEN abap_true ELSE abap_false ).

*     result = VALUE #( for travel IN travels
*                      LET lv_flag = COND #( when travel-Status = 'X' THEN if_abap_behv=>auth-unauthorized
*                                            else if_abap_behv=>auth-allowed )
*                                    IN ( %tky = travel-%tky
*                                         %update = lv_flag ) ).

    LOOP AT travels INTO DATA(ls_travel).
    IF  is_update_requested = abap_true.

    IF is_update_granted( has_before_image =  ' ' overall_status = 'X' ) = abap_false.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky = ls_travel-%tky
                        %msg = new_message_with_text(
                        severity = if_abap_behv_message=>severity-error
                        text     = 'No Aithorization to edit !' )
                      ) TO reported-travel.
    ENDIF.
     ENDIF.

     APPEND VALUE #( %tky = ls_travel-%tky

                       %update              = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                       %action-acceptTravel = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                       %action-rejectTravel = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
*                      %action-Prepare      = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-Edit         = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                       %assoc-_Booking      = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )

                       %delete              = COND #( WHEN delete_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                     )
         TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD precheck_update.
    LOOP AT entities INTO DATA(travel).
      CHECK travel-%control-CurrencyCode EQ '01'.
      IF travel-currencycode IS NOT INITIAL.

        SELECT SINGLE * FROM i_currency
            WHERE currency = @travel-currencycode
            INTO @DATA(ls_currrency).
        IF sy-subrc NE 0.
          APPEND VALUE #( %key = travel-%key
                          %update = if_abap_behv=>mk-on ) TO failed-travel.
          APPEND VALUE #( %key = travel-%key
                          %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                        text = 'Currency Code is not valid2' )
                          %update = if_abap_behv=>mk-on
                          %element-currencycode = if_abap_behv=>mk-on
                         ) TO reported-travel.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_create_granted.

  ENDMETHOD.

  METHOD is_delete_granted.

  ENDMETHOD.

  METHOD is_update_granted.

   update_granted = abap_false.

  ENDMETHOD.

  METHOD get_global_features.
    DATA(time1) = CONV t( '010000' ).
    DATA(time2) = CONV t( '240000' ).

    result = VALUE #( %delete = COND #(
                WHEN cl_abap_context_info=>get_system_time( )
                  BETWEEN time1 AND time2
                THEN if_abap_behv=>fc-o-disabled
                ELSE if_abap_behv=>fc-o-enabled )
                    ).
*
*  IF result-%delete = if_abap_behv=>fc-o-disabled.
*    APPEND VALUE #( %msg  = new_message_with_text(
*          text     = 'Delete is only allowed between 10 pm and 6 am.'
*          severity = if_abap_behv_message=>severity-error )
*          %global = if_abap_behv=>mk-on )
*          TO reported-travel.
*  ENDIF.
*    RESULT-%delete = '01'.
  ENDMETHOD.

ENDCLASS.
