Describe 'IO.Swagger StoreApi' {
    Context 'StoreApi' {
        It 'Invoke-StoreApiDeleteOrder' {
            $ret = Invoke-PetApiGetPetById -orderId 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'StoreApi' {
        It 'Invoke-StoreApiGetInventory' {
            $ret = Invoke-PetApiGetPetById
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'StoreApi' {
        It 'Invoke-StoreApiGetOrderById' {
            $ret = Invoke-PetApiGetPetById -orderId 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

    Context 'StoreApi' {
        It 'Invoke-StoreApiPlaceOrder' {
            $ret = Invoke-PetApiGetPetById -body 
            #$ret | Should BeOfType IO.Swagger.Model.ModelNameHere
            #$ret.property | Should Be 0
        }
    }

}
