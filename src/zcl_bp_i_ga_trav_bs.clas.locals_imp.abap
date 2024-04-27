CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel.
    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel.
    METHODS setInitialStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setInitialStatus.
    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.
    METHODS setTravelId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Travel~setTravelId.
*    METHODS get_instance_features FOR INSTANCE FEATURES
*      IMPORTING keys REQUEST requested_features FOR Travel RESULT result.

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
        LOOP at travel INTO DATA(ls_travel).
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky = ls_travel-%tky %msg = new_message_with_text(  severity = IF_ABAP_BEHV_MESSAGE=>SEVERITY-ERROR
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

ENDCLASS.
