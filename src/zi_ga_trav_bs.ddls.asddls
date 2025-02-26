@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Travel Interface View'
//@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_GA_TRAV_BS
  as select from zga_trav_bs as Travel
  composition [0..*] of ZI_GA_BOOK_BS   as _Booking
  association [0..1] to /DMO/I_Agency   as _Agency   on $projection.AgencyId = _Agency.AgencyID
  association [0..1] to /DMO/I_Customer as _Customer on $projection.CustomerId = _Customer.CustomerID
  association [0..1] to I_Currency      as _Currency on $projection.CurrencyCode = _Currency.Currency
{
  key travel_uuid           as TravelUuid,
      travel_id             as TravelId,
      agency_id             as AgencyId,
      customer_id           as CustomerId,
      begin_date            as BeginDate,
      end_date              as EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      booking_fee           as BookingFee,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      total_price           as TotalPrice,
      currency_code         as CurrencyCode,
      description           as Description,
      status                as Status,
      case status
        when 'O' then 2
        when 'A' then 3
        when 'X' then 1
      else 0
      end                   as OverallStatusCriticality,
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
      /* Associations*/
      _Agency,
      _Customer,
      _Currency,
      _Booking
}
