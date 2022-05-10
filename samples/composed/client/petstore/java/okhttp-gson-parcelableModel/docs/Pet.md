# Pet

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **Long** |  |  [optional]
**name** | **String** |  | 
**status** | [**StatusEnum**](#StatusEnum) | pet status in the store |  [optional]
**part** | **List&lt;OneOfPetPartItems&gt;** |  |  [optional]

<a name="StatusEnum"></a>
## Enum: StatusEnum
Name | Value
---- | -----
AVAILABLE | &quot;available&quot;
PENDING | &quot;pending&quot;
SOLD | &quot;sold&quot;
