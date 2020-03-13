'use strict';

var utils = require('../utils/writer.js');
var Store = require('../service/StoreService');

module.exports.deleteOrder = function deleteOrder (req, res, next, order_id) {
  Store.deleteOrder(order_id)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getInventory = function getInventory (req, res, next) {
  Store.getInventory()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getOrderById = function getOrderById (req, res, next, order_id) {
  Store.getOrderById(order_id)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.placeOrder = function placeOrder (req, res, next, body) {
  Store.placeOrder(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
