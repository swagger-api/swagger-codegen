Describe 'IO.Swagger PetApi' {
    Context 'PetApi' {
        It 'Invoke-PetApiAddPet' {
            $ret = Invoke-PetApiGetPetById -body 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'PetApi' {
        It 'Invoke-PetApiDeletePet' {
            $ret = Invoke-PetApiGetPetById -petId  -apiKey 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'PetApi' {
        It 'Invoke-PetApiFindPetsByStatus' {
            $ret = Invoke-PetApiGetPetById -status 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'PetApi' {
        It 'Invoke-PetApiFindPetsByTags' {
            $ret = Invoke-PetApiGetPetById -tags 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'PetApi' {
        It 'Invoke-PetApiGetPetById' {
            $ret = Invoke-PetApiGetPetById -petId 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'PetApi' {
        It 'Invoke-PetApiUpdatePet' {
            $ret = Invoke-PetApiGetPetById -body 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'PetApi' {
        It 'Invoke-PetApiUpdatePetWithForm' {
            $ret = Invoke-PetApiGetPetById -petId  -name  -status 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'PetApi' {
        It 'Invoke-PetApiUploadFile' {
            $ret = Invoke-PetApiGetPetById -petId  -additionalMetadata  -file 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

}
