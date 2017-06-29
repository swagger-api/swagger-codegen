using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// Model for testing model with \&quot;_class\&quot; property
    /// </summary>
    public sealed class ClassModel:  IEquatable<ClassModel>
    { 
        /// <summary>
        /// Class
        /// </summary>
        public string Class { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use ClassModel.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public ClassModel()
        {
        }

        private ClassModel(string Class)
        {
            
            this.Class = Class;
            
        }

        /// <summary>
        /// Returns builder of ClassModel.
        /// </summary>
        /// <returns>ClassModelBuilder</returns>
        public static ClassModelBuilder Builder()
        {
            return new ClassModelBuilder();
        }

        /// <summary>
        /// Returns ClassModelBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>ClassModelBuilder</returns>
        public ClassModelBuilder With()
        {
            return Builder()
                .Class(Class);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(ClassModel other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (ClassModel.
        /// </summary>
        /// <param name="left">Compared (ClassModel</param>
        /// <param name="right">Compared (ClassModel</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (ClassModel left, ClassModel right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (ClassModel.
        /// </summary>
        /// <param name="left">Compared (ClassModel</param>
        /// <param name="right">Compared (ClassModel</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (ClassModel left, ClassModel right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of ClassModel.
        /// </summary>
        public sealed class ClassModelBuilder
        {
            private string _Class;

            internal ClassModelBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for ClassModel.Class property.
            /// </summary>
            /// <param name="value">Class</param>
            public ClassModelBuilder Class(string value)
            {
                _Class = value;
                return this;
            }


            /// <summary>
            /// Builds instance of ClassModel.
            /// </summary>
            /// <returns>ClassModel</returns>
            public ClassModel Build()
            {
                Validate();
                return new ClassModel(
                    Class: _Class
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}
