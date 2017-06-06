function PetApi-addPet {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: PetApi-addPet' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:PetApi.addPet(
            ${ body }
        )
    }
}

function PetApi-deletePet {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: PetApi-deletePet' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:PetApi.deletePet(
            ${ petId },
            ${ apiKey }
        )
    }
}

function PetApi-findPetsByStatus {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: PetApi-findPetsByStatus' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:PetApi.findPetsByStatus(
            ${ status }
        )
    }
}

function PetApi-findPetsByTags {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: PetApi-findPetsByTags' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:PetApi.findPetsByTags(
            ${ tags }
        )
    }
}

function PetApi-getPetById {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: PetApi-getPetById' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:PetApi.getPetById(
            ${ petId }
        )
    }
}

function PetApi-updatePet {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: PetApi-updatePet' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:PetApi.updatePet(
            ${ body }
        )
    }
}

function PetApi-updatePetWithForm {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: PetApi-updatePetWithForm' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:PetApi.updatePetWithForm(
            ${ petId },
            ${ name },
            ${ status }
        )
    }
}

function PetApi-uploadFile {
    [CmdletBinding()]
    Param (
    {{#allParams}
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [.Model.Pet]
        ${  }
    {{/allParams}
    )

    Process {
        'Calling method: PetApi-uploadFile' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:PetApi.uploadFile(
            ${ petId },
            ${ additionalMetadata },
            ${ file }
        )
    }
}

