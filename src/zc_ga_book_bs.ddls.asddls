@EndUserText.label: 'Booking Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
@Metadata.allowExtensions: true
define view entity ZC_GA_BOOK_BS
  as projection on ZI_GA_BOOK_BS as Booking
{
  key BookingUuid,
      TravelUuid,
      TravelId,
      @Search.defaultSearchElement: true
      BookingId,
      BookingDate,
      @Search.defaultSearchElement: true
      @EndUserText.label: 'Customer'
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Customer', element: 'CustomerID' } }]
      @ObjectModel.text.element: [ 'CustomerName' ]
      CustomerId,
      _Customer.FirstName as CustomerName,
      @EndUserText.label: 'Airline'
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Carrier', element: 'AirlineID' } }]
      @ObjectModel.text.element: [ 'AirlineName'  ]
      CarrierId,
      _Carrier.Name as AirlineName,
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Flight', element: 'ConnectionID' },
                                           additionalBinding: [{ localElement: 'CarrierId', element: 'AirlineID' },
                                                               { localElement: 'FlightDate', element: 'FlightDate' },
                                                               { localElement: 'FlightPrice', element: 'Price'},
                                                               { localElement: 'CurrencyCode', element: 'CurrencyCode' }]                                                   
                                                     }]
      @ObjectModel.text.element: [ 'AirlineName'  ] 
      ConnectionId,
      FlightDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      FlightPrice,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_Currency' , element: 'Currency' } }]
      CurrencyCode,
      LocalLastChangedAt,
      /* Associations */
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _Flight,
      _Travel : redirected to parent ZC_GA_TRAV_BS
}
