library swagger.api;

import 'dart:async';
import 'dart:convert';
import 'package:http/browser_client.dart';
import 'package:http/http.dart';

part 'api_client.dart';
part 'api_helper.dart';
part 'api_exception.dart';
part 'auth/authentication.dart';
part 'auth/api_key_auth.dart';
part 'auth/oauth.dart';
part 'auth/http_basic_auth.dart';

part 'api/default_api.dart';
part 'api/pet_api.dart';
part 'api/store_api.dart';
part 'api/user_api.dart';
part 'model/all_of_sub_category_category.dart';
part 'model/all_pets_response.dart';
part 'model/api_response.dart';
part 'model/body.dart';
part 'model/cat.dart';
part 'model/cat_all_of2.dart';
part 'model/category.dart';
part 'model/dog.dart';
part 'model/dog_all_of2.dart';
part 'model/inline_array_items_all_pets_response.dart';
part 'model/nullable_enum_model.dart';
part 'model/one_of_single_pet_response_pet.dart';
part 'model/one_ofinline_array_items_all_pets_response.dart';
part 'model/order.dart';
part 'model/pet.dart';
part 'model/single_pet_response.dart';
part 'model/sub_category.dart';
part 'model/sub_category_pets.dart';
part 'model/tag.dart';
part 'model/test.dart';
part 'model/user.dart';

ApiClient defaultApiClient = new ApiClient();
