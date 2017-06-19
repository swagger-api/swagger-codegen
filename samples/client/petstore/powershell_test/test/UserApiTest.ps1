Describe 'IO.Swagger UserApi' {
    Context 'UserApi' {
        It 'Invoke-UserApiCreateUser' {
            $ret = Invoke-PetApiGetPetById -body 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'UserApi' {
        It 'Invoke-UserApiCreateUsersWithArrayInput' {
            $ret = Invoke-PetApiGetPetById -body 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'UserApi' {
        It 'Invoke-UserApiCreateUsersWithListInput' {
            $ret = Invoke-PetApiGetPetById -body 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'UserApi' {
        It 'Invoke-UserApiDeleteUser' {
            $ret = Invoke-PetApiGetPetById -username 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'UserApi' {
        It 'Invoke-UserApiGetUserByName' {
            $ret = Invoke-PetApiGetPetById -username 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'UserApi' {
        It 'Invoke-UserApiLoginUser' {
            $ret = Invoke-PetApiGetPetById -username  -password 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'UserApi' {
        It 'Invoke-UserApiLogoutUser' {
            $ret = Invoke-PetApiGetPetById
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'UserApi' {
        It 'Invoke-UserApiUpdateUser' {
            $ret = Invoke-PetApiGetPetById -username  -body 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

}
