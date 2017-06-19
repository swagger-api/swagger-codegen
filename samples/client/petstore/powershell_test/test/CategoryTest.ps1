## TODO we need to update the template to test the model files

function New-Category {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Int64]]
        ${id},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${name}
    )

    Process {
        'Creating object: IO.Swagger.Model.Category' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        New-Object -TypeName IO.Swagger.Model.Category -ArgumentList @(
            ${id},
            ${name}
        )
    }
}
