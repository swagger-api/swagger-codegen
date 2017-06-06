function StoreApi-deleteOrder {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: StoreApi-deleteOrder' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.deleteOrder(
            ${ orderId }
        )
    }
}

function StoreApi-getInventory {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
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
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: StoreApi-getOrderById' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.getOrderById(
            ${ orderId }
        )
    }
}

function StoreApi-placeOrder {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: StoreApi-placeOrder' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.placeOrder(
            ${ body }
        )
    }
}

