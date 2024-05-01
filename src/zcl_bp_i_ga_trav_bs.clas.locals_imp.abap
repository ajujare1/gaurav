CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel RESULT result.
    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel RESULT result.
    METHODS setInitialStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setInitialStatus.
    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.
    METHODS setTravelId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Travel~setTravelId.
    METHODS setTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setTotalPrice.
    METHODS recalcTotalPrice FOR MODIFY
      IMPORTING keys FOR ACTION Travel~recalcTotalPrice.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR travel RESULT result.
    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR travel RESULT result.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR travel RESULT result.
*    METHODS precheck_update FOR PRECHECK
*      IMPORTING entities FOR UPDATE travel.
    METHODS is_update_granted IMPORTING iv_existing           TYPE abap_bool
                                        iv_status             TYPE /dmo/overall_status
                              RETURNING VALUE(update_granted) TYPE abap_bool.


ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD acceptTravel.
    MODIFY ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
     ENTITY Travel
     UPDATE
         FIELDS ( Status )
         WITH VALUE #( FOR ls_key IN keys
                         ( %tky = ls_key-%tky
                           Status = 'A' ) )
         FAILED failed
         REPORTED reported.

    READ ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
    ENTITY Travel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels
                      (  %tky = travel-%tky
                           %param = travel
                      ) ).

  ENDMETHOD.

  METHOD rejectTravel.
    MODIFY ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
   ENTITY Travel
   UPDATE
       FIELDS ( Status )
       WITH VALUE #( FOR ls_key IN keys
                       ( %tky = ls_key-%tky
                         Status = 'X' ) )
       FAILED failed
       REPORTED reported.

    READ ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
   ENTITY Travel
   ALL FIELDS WITH CORRESPONDING #( keys )
   RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels
                      (  %tky = travel-%tky
                           %param = travel
                      ) ).

  ENDMETHOD.

*  METHOD get_instance_features.
*  ENDMETHOD.

  METHOD setInitialStatus.
    MODIFY ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
      ENTITY Travel
      UPDATE
          FIELDS ( Status )
          WITH VALUE #( FOR ls_key IN keys
                          ( %tky = ls_key-%tky
                            Status = 'O' ) )
          FAILED DATA(failed)
          REPORTED DATA(reported_1).
  ENDMETHOD.

  METHOD validateAgency.
    DATA lt_agency TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.
    READ
     ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
     ENTITY Travel
        FIELDS ( AgencyID ) WITH CORRESPONDING #( keys )
        RESULT DATA(travel).

    lt_agency = CORRESPONDING #( travel DISCARDING DUPLICATES MAPPING agency_id = AgencyId ) .
    DELETE lt_agency WHERE agency_id IS INITIAL.
    IF lt_agency IS INITIAL.
      LOOP AT travel INTO DATA(ls_travel).
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky = ls_travel-%tky %msg = new_message_with_text(  severity = if_abap_behv_message=>severity-error
                                                                             text = 'Agency ID can not be empty' ) )


        TO reported-travel.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD setTravelId.
    READ
   ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
   ENTITY Travel
      FIELDS ( TravelID ) WITH CORRESPONDING #( keys )
      RESULT DATA(travel).

    DELETE travel WHERE TravelId IS NOT INITIAL.
    CHECK travel IS NOT INITIAL.

    SELECT SINGLE MAX( travel_id ) AS travelId
    FROM zga_trav_bs
    INTO @DATA(last_id).

    MODIFY ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
    ENTITY Travel
    UPDATE FROM VALUE #( FOR ls_travel IN Travel (
                      %tky = ls_travel-%tky
                      TravelId = last_id + sy-tabix
                      %control-TravelId = '01'
                      )
     ).

  ENDMETHOD.

  METHOD setTotalPrice.

    MODIFY ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
    ENTITY Travel
    EXECUTE recalcTotalPrice
    FROM CORRESPONDING #( keys )
    REPORTED DATA(set_reported).

    reported = CORRESPONDING #( DEEP set_reported ).

  ENDMETHOD.


  METHOD recalcTotalPrice.

    TYPES: BEGIN OF ty_price,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_price.

    DATA : lt_price TYPE STANDARD TABLE OF ty_price.
*  Read Booking Fee
    READ ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
    ENTITY Travel
      FIELDS ( BookingFee CurrencyCode )
      WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).

      lt_price = VALUE #( ( amount = <travel>-BookingFee
                            currency_code = <travel>-CurrencyCode
                           ) ) .
*  Read Flight Details
      READ ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
      ENTITY Travel BY \_Booking
       FIELDS ( FlightPrice CurrencyCode )
       WITH VALUE #( (  %tky = <travel>-%tky ) )
       RESULT DATA(bookings).

      LOOP AT bookings ASSIGNING FIELD-SYMBOL(<booking>).
        COLLECT VALUE ty_price(  amount = <booking>-FlightPrice
                           currency_code = <booking>-CurrencyCode  ) INTO lt_price.
      ENDLOOP.

* Calculating Total Price
      LOOP AT lt_price INTO DATA(ls_price).
        <travel>-TotalPrice += ls_price-amount.
      ENDLOOP.
    ENDLOOP.

* Modify Total Price
    MODIFY ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
    ENTITY Travel
     UPDATE FIELDS ( TotalPrice )
     WITH CORRESPONDING #( travels ).

  ENDMETHOD.

  METHOD get_instance_authorizations.
    READ ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
     ENTITY travel
     FIELDS ( status )
     WITH CORRESPONDING #( keys )
     RESULT DATA(travels).

    DATA(is_update_req) = COND #( WHEN requested_authorizations-%update = '01' OR
                                        requested_authorizations-%action-acceptTravel = '01' OR
                                        requested_authorizations-%action-rejectTravel = '01' OR
                                        requested_authorizations-%action-Edit = '01' OR
                                        requested_authorizations-%assoc-_Booking = '01'

                                        THEN abap_true ELSE abap_false  ).

    LOOP AT travels INTO DATA(ls_travel).
      IF is_update_req = abap_true.
        DATA(update_granted) = is_update_granted( iv_existing = abap_true iv_status = ls_travel-Status  ).
        IF update_granted = abap_false.
          APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
          APPEND VALUE #( %tky = ls_travel-%tky
                          %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                        text = 'No Authorization ! please request access' )

                          ) TO reported-travel.
        ENDIF.
      ENDIF.

      APPEND VALUE #( %tky = ls_travel-%tky
                      %update = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-allowed )
                      %action-acceptTravel = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-allowed )
                      %action-rejectTravel = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-allowed )
                      %action-Edit = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-allowed )
                      %assoc-_Booking = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-allowed )
      ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_features.
    "Read Status
    READ ENTITIES OF zi_ga_trav_bs IN LOCAL MODE
    ENTITY Travel
    FIELDS ( Status ) WITH CORRESPONDING #( keys )
    RESULT DATA(travels).

    result = VALUE #(
              FOR travel IN travels
                ( %tky = travel-%tky
                  %action-acceptTravel = COND #( WHEN travel-status = 'A' THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled
                                                 )
                  %action-rejectTravel =  COND #( WHEN travel-status = 'X' THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled
                                                 )
*                  %delete = COND #( WHEN travel-status = 'A' THEN if_abap_behv=>fc-o-disabled
*                                                 ELSE if_abap_behv=>fc-o-enabled
*                                                 )
                  %assoc-_Booking = COND #( WHEN travel-status = 'A' THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled
                                                 )
                 )
               ).
  ENDMETHOD.

  METHOD get_global_features.
    DATA(time_from) = CONV t( '230000' ).
    DATA(time_to) = CONV t( '240000' ).

    result-%delete = COND #( WHEN cl_abap_context_info=>get_system_time(  )
                             BETWEEN time_from AND time_to
                             THEN if_abap_behv=>fc-o-enabled
                             ELSE if_abap_behv=>fc-o-disabled ).

*    DATA(lv_time) = cl_abap_context_info=>get_system_time(  ).
*    IF lv_time > time_from AND lv_time < time_to.
*    result-%delete = if_abap_behv=>fc-o-enabled.
*    ELSE.
*     result-%delete = if_abap_behv=>fc-o-disabled.
*    ENDIF.
  ENDMETHOD.



  METHOD get_global_authorizations.

  ENDMETHOD.

*  METHOD precheck_update.
*
*    LOOP AT entities INTO DATA(travel).
*      IF travel-CurrencyCode IS NOT INITIAL.
*
*        SELECT SINGLE * FROM I_Currency
*          WHERE Currency = @travel-CurrencyCode
*              INTO @DATA(ls_currency).
*
*          IF sy-subrc IS NOT INITIAL.
*            APPEND VALUE #( %tky = travel-%tky
*                           %update = '01'
*                           ) TO failed-travel.
*            APPEND VALUE #( %tky = travel-%tky
*                            %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Currency Code Precheck Not Valid' )
*
*
*             ) to reported-travel.
*
*          ENDIF.
*
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDMETHOD.

  METHOD is_update_granted.
    IF iv_existing = abap_true.
      AUTHORITY-CHECK OBJECT 'ZOSTATUS'
      ID 'ZOSTATUS' FIELD iv_status
      ID 'ACTVT' FIELD '02'
      .
    ELSE.
      AUTHORITY-CHECK OBJECT 'ZOSTATUS'
        ID 'ZOSTATUS' DUMMY
        ID 'ACTVT' FIELD '02'
        .
    ENDIF.
    IF sy-subrc IS INITIAL.
      update_granted = abap_true.
    ELSE.
      update_granted = abap_false.
    ENDIF.
    update_granted = abap_true.
  ENDMETHOD.

ENDCLASS.
