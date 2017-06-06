function PetApi-addPet {
    [CmdletBinding()]
    Param (
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ body }
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
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ petId }
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [IO.Swagger.Model.Pet]
        ${ apiKey }
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
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ status }
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
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ tags }
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
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ petId }
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
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ body }
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
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ petId }
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [IO.Swagger.Model.Pet]
        ${ name }
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [IO.Swagger.Model.Pet]
        ${ status }
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
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ petId }
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [IO.Swagger.Model.Pet]
        ${ additionalMetadata }
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [IO.Swagger.Model.Pet]
        ${ file }
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

