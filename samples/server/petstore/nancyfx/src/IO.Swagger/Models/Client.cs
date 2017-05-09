using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// Client
    /// </summary>
    public sealed class Client:  IEquatable<Client>
    { 
        /// <summary>
        /// _Client
        /// </summary>
        public string _Client { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Client.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Client()
        {
        }

        private Client(string _Client)
        {
            
            this._Client = _Client;
            
        }

        /// <summary>
        /// Returns builder of Client.
        /// </summary>
        /// <returns>ClientBuilder</returns>
        public static ClientBuilder Builder()
        {
            return new ClientBuilder();
        }

        /// <summary>
        /// Returns ClientBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>ClientBuilder</returns>
        public ClientBuilder With()
        {
            return Builder()
                ._Client(_Client);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Client other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Client.
        /// </summary>
        /// <param name="left">Compared (Client</param>
        /// <param name="right">Compared (Client</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Client left, Client right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Client.
        /// </summary>
        /// <param name="left">Compared (Client</param>
        /// <param name="right">Compared (Client</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Client left, Client right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Client.
        /// </summary>
        public sealed class ClientBuilder
        {
            private string __Client;

            internal ClientBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for Client._Client property.
            /// </summary>
            /// <param name="value">_Client</param>
            public ClientBuilder _Client(string value)
            {
                __Client = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Client.
            /// </summary>
            /// <returns>Client</returns>
            public Client Build()
            {
                Validate();
                return new Client(
                    _Client: __Client
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}
