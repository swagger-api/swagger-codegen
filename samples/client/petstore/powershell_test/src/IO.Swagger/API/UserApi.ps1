function UserApi-createUser {
    [CmdletBinding()]
    Param (
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ body }
    )

    Process {
        'Calling method: UserApi-createUser' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.createUser(
            ${ body }
        )
    }
}

function UserApi-createUsersWithArrayInput {
    [CmdletBinding()]
    Param (
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ body }
    )

    Process {
        'Calling method: UserApi-createUsersWithArrayInput' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.createUsersWithArrayInput(
            ${ body }
        )
    }
}

function UserApi-createUsersWithListInput {
    [CmdletBinding()]
    Param (
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ body }
    )

    Process {
        'Calling method: UserApi-createUsersWithListInput' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.createUsersWithListInput(
            ${ body }
        )
    }
}

function UserApi-deleteUser {
    [CmdletBinding()]
    Param (
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ username }
    )

    Process {
        'Calling method: UserApi-deleteUser' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.deleteUser(
            ${ username }
        )
    }
}

function UserApi-getUserByName {
    [CmdletBinding()]
    Param (
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ username }
    )

    Process {
        'Calling method: UserApi-getUserByName' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.getUserByName(
            ${ username }
        )
    }
}

function UserApi-loginUser {
    [CmdletBinding()]
    Param (
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ username }
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ password }
    )

    Process {
        'Calling method: UserApi-loginUser' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.loginUser(
            ${ username },
            ${ password }
        )
    }
}

function UserApi-logoutUser {
    [CmdletBinding()]
    Param (
    )

    Process {
        'Calling method: UserApi-logoutUser' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.logoutUser(
        )
    }
}

function UserApi-updateUser {
    [CmdletBinding()]
    Param (
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ username }
        [Parameter(Position = , ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Pet]
        ${ body }
    )

    Process {
        'Calling method: UserApi-updateUser' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.updateUser(
            ${ username },
            ${ body }
        )
    }
}

