@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Interface View'

define view entity ZI_GA_BOOK_BS
  as select from zga_book_bs as Booking

  association        to parent ZI_GA_TRAV_BS as _Travel     on  $projection.TravelUuid = _Travel.TravelUuid
  association [0..1] to /DMO/I_Customer      as _Customer   on  $projection.CustomerId = _Customer.CustomerID
  association [0..1] to /DMO/I_Carrier       as _Carrier    on  $projection.CarrierId = _Carrier.AirlineID
  association [0..1] to /DMO/I_Connection    as _Connection on  $projection.CarrierId    = _Connection.AirlineID
                                                            and $projection.ConnectionId = _Connection.ConnectionID
  association [0..1] to /DMO/I_Flight        as _Flight     on  $projection.CarrierId    = _Flight.AirlineID
                                                            and $projection.ConnectionId = _Flight.ConnectionID
                                                            and $projection.FlightDate   = _Flight.FlightDate
  association [0..1] to I_Currency           as _Currency   on  $projection.CurrencyCode = _Currency.Currency

{
  key booking_uuid          as BookingUuid,
      travel_uuid           as TravelUuid,
      travel_id             as TravelId,
      booking_id            as BookingId,
      booking_date          as BookingDate,
      customer_id           as CustomerId,
      carrier_id            as CarrierId,
      connection_id         as ConnectionId,
      flight_date           as FlightDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      flight_price          as FlightPrice,
      currency_code         as CurrencyCode,
      @Semantics.user.createdBy: true
      createdby             as Createdby,
      @Semantics.systemDateTime.createdAt: true
      createdat             as Createdat,
      @Semantics.user.lastChangedBy: true
      lastchangedby         as Lastchangedby,
      @Semantics.systemDateTime.lastChangedAt: true
      lastchangedat         as Lastchangedat,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,

      /*Associations*/
      _Customer,
      _Carrier,
      _Connection,
      _Flight,
      _Currency,
      _Travel

}
