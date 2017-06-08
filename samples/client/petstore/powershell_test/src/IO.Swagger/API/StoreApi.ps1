function StoreApi-deleteOrder {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 1, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${orderId}
    )

    Process {
        'Calling method: StoreApi-deleteOrder' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.deleteOrder(
            ${orderId}
        )
    }
}

function StoreApi-getInventory {
    [CmdletBinding()]
    Param (
    )

    Process {
        'Calling method: StoreApi-getInventory' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.getInventory(
        )
    }
}

function StoreApi-getOrderById {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 1, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Int64]
        ${orderId}
    )

    Process {
        'Calling method: StoreApi-getOrderById' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.getOrderById(
            ${orderId}
        )
    }
}

function StoreApi-placeOrder {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 1, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Order]
        ${body}
    )

    Process {
        'Calling method: StoreApi-placeOrder' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.placeOrder(
            ${body}
        )
    }
}

