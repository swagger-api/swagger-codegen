# Pet

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | [**kotlin.Long**](.md) |  |  [optional]
**category** | [**Category**](Category.md) |  |  [optional]
**name** | [**kotlin.String**](.md) |  | 
**photoUrls** | [**kotlin.Array&lt;kotlin.String&gt;**](.md) |  | 
**tags** | [**kotlin.Array&lt;Tag&gt;**](Tag.md) |  |  [optional]
**status** | [**inline**](#StatusEnum) | pet status in the store |  [optional]

<a name="StatusEnum"></a>
## Enum: status
Name | Value
---- | -----
status | available, pending, sold
