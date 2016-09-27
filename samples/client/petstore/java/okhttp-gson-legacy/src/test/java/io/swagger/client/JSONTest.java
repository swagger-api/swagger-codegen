package io.swagger.client;

import com.google.gson.reflect.TypeToken;

import io.swagger.client.model.Order;

import java.lang.Exception;
import java.lang.reflect.Type;
import java.sql.Time;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import org.junit.*;
import static org.junit.Assert.*;

public class JSONTest {
    ApiClient apiClient = null;
    JSON json = null;
    Order order = null;

    @Before
    public void setup() {
        apiClient = new ApiClient();
        json = new JSON(apiClient);
        order = new Order();
    }

    @Test
    public void testDefaultDate() throws Exception {
        final SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
        final String dateStr = "2015-11-07T14:11:05.267Z";
        order.setShipDate(simpleDateFormat.parse(dateStr));

        String str = json.serialize(order);
        Type type = new TypeToken<Order>() { }.getType();
        Order o = json.deserialize(str, type);
        assertEquals(dateStr, simpleDateFormat.format(o.getShipDate()));
    }

    @Test
    public void testCustomDate() throws Exception {
        final SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
        simpleDateFormat.setTimeZone(TimeZone.getTimeZone("Etc/GMT+2"));
        final String dateStr = "2015-11-07T14:11:05-02:00";
        order.setShipDate(simpleDateFormat.parse(dateStr));

        String str = json.serialize(order);
        Type type = new TypeToken<Order>() { }.getType();
        Order o = json.deserialize(str, type);
        assertEquals(dateStr, simpleDateFormat.format(o.getShipDate()));
    }
}