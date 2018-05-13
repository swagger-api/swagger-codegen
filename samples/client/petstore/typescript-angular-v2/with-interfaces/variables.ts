import { OpaqueToken } from '@angular/core';
import {Observable} from 'rxjs';

export const BASE_PATH = new OpaqueToken('basePath');
export const BASE_PATH_OBSERVABLE = new OpaqueToken('basePathObservable');
export const COLLECTION_FORMATS = {
    'csv': ',',
    'tsv': '   ',
    'ssv': ' ',
    'pipes': '|'
}
