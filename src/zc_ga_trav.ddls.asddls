@EndUserText.label: 'Travel Projection View'
@Search.searchable: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.semanticKey: [ 'TravelId']
@Metadata.allowExtensions: true
define root view entity ZC_GA_TRAV
provider contract transactional_query
  as projection on ZI_GA_TRAV
{
  key TravelUuid,
      @Search.defaultSearchElement: true
      TravelId,
      @Search.defaultSearchElement: true
      @EndUserText.label: 'Agency'
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Agency', element: 'AgencyID' },
                                           useForValidation: true }]
      @ObjectModel.text.element: [ 'AgencyName' ]
      AgencyId,
      _Agency.Name as AgencyName,
      @EndUserText.label: 'Customer'
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Customer', element: 'CustomerID' } }]
      @ObjectModel.text.element: [ 'CustomerName' ]
      CustomerId,
      _Customer.FirstName as CustomerName,
      BeginDate,
      EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      BookingFee,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      TotalPrice,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_Currency', element: 'Currency' } }]
      CurrencyCode,
      Description,
      Status,
      OverallStatusCriticality,
      Lastchangedat,
      LocalLastChangedAt,
      /* Associations */
      _Agency,
      _Currency,
      _Customer,
      
      _Booking : redirected to composition child ZC_GA_BOOK
}
